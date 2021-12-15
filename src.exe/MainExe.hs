{-# LANGUAGE TemplateHaskell #-}
module Main
       ( main, mainArgs
       ) where

import qualified Data.Map.Strict    as M
import           System.Environment (getArgs, getExecutablePath)
import           System.Exit        (ExitCode (..))
import           System.Process     (readProcessWithExitCode)


import           Imports
import qualified MainExeTH
import           Report             (DayIndex, DayInput, DayKey,
                                     ExecResult (..))
import qualified Report
import qualified SysInfo
import           Util

import qualified Y15.D01
import qualified Y15.D02
import qualified Y15.D03
import qualified Y15.D04
import qualified Y15.D05
import qualified Y15.D06
import qualified Y15.D07
import qualified Y15.D08
import qualified Y15.D09
import qualified Y15.D10
import qualified Y15.D11
import qualified Y15.D12
import qualified Y15.D13
import qualified Y15.D14
import qualified Y15.D15
import qualified Y15.D16
import qualified Y15.D17
import qualified Y15.D18
import qualified Y15.D19
import qualified Y15.D20
import qualified Y15.D21

import qualified Y21.D01
import qualified Y21.D02
import qualified Y21.D03

days :: Map String [String -> IO String]
days = M.fromList $ join $(MainExeTH.solversFromImportsAndSources)

-- # day     answer-1  answer-2
-- Y15.D01   138       1771
-- Y15.D02   1586300   3737498
{-# ANN parseAnswers ("HLint: ignore Use map once" :: String) #-}
parseAnswers :: String -> Map String [String]
parseAnswers =
    M.fromList
    . map (\case
            day:answers -> (day, answers)
            x           -> error $ "Couldn't parse answers: " ++ show x)
    . map (filter (/= "") . splitOn " ")
    . filter (/= "")
    . filter (not . isPrefixOf "#")
    . lines

main :: IO ()
main = getArgs >>= mainArgs

mainArgs :: [String] -> IO ()
mainArgs args =
    case args of
        ["runday", dayKey, dayNs] ->
            case M.lookup dayKey days of
                Nothing      -> error $ "Couldn't find day " ++ dayKey ++ ", solver " ++ dayNs
                Just solvers -> readInput dayKey >>= (solvers !! read dayNs) >>= putStr

        _ -> do
            -- select day(s)
            let daysPred = case args of
                    []  -> const True
                    [x] -> (x `isPrefixOf`)
                    _   -> error $ "Don't know how to interpret args: " ++ show args

            let daysSelected = M.toList $ M.filterWithKey (\k _ -> daysPred k) days

            when (null daysSelected) $
                error $ "Couldn't find day " ++ (show . head) args
                    ++ ".\nAvailable days are: " ++ intercalate ", " (M.keys days) ++ ".\n"

            -- read answers.txt
            let answersPath = "res/answers.txt"
            day2answers <- parseAnswers <$> readFile answersPath

            Report.header

            -- run
            forM_ daysSelected $ \ (dayKey, solvers) -> do
                input <- readInput dayKey

                Report.dayPrefix dayKey

                results <- forM (take (length solvers) [0..]) $ \solverIndex -> do
                    runDayViaExec input dayKey solverIndex >>= \case
                        -- TODO 1. correctly present error (Left) and continue with the rest of days
                        -- TODO 2. find a better way to remove RTS part that starts with " [("
                        Left err -> error $
                                "solver with index "
                                ++ show solverIndex ++ " failed: "
                                ++ (unlines . takeWhile (\e -> not $ " [(" `isPrefixOf` e) . lines $ err)
                        Right r -> return r

                Report.dayResults answersPath day2answers dayKey results

            Report.footer =<< SysInfo.getSysInfo

-- TODO report failures
runDayViaDirectCall :: DayInput -> DayKey -> DayIndex -> IO (Either String ExecResult)
runDayViaDirectCall input dayKey dayIndex = do
    let ioa = fromMaybe (error $ "Couldn't find day: " ++ dayKey) (M.lookup dayKey days) !! dayIndex
    (out, ms) <- timeOf $ ioa input
    return . Right $ ExecResult out ms Nothing Nothing

runDayViaExec :: DayInput -> DayKey -> DayIndex -> IO (Either String ExecResult)
runDayViaExec input dayKey dayIndex = do
    selfPath <- getExecutablePath

    if "ghc" `isSuffixOf` selfPath
        -- fallback to direct call if we are in a REPL
        then runDayViaDirectCall input dayKey dayIndex
        else do
            (e, out, err) <- readProcessWithExitCode
                selfPath
                ["runday", dayKey, show dayIndex, "+RTS", "-t", "--machine-readable", "-RTS"]
                input

            case e of
                -- TODO move error to stdout? (to separate from stats in stderr)
                ExitFailure _ -> return . Left $ err
                ExitSuccess -> do
                    -- TODO handle parse error in stdout
                    let s :: Map String String = M.fromList $ read err

                    -- total_cpu_seconds, total_wall_seconds
                    --   mut_cpu_seconds,   mut_wall_seconds
                    return . Right $ ExecResult
                        out
                        (maybe (-1) (ceiling . (* 1000))
                            (M.lookup "total_wall_seconds" s <&> (read :: String -> Double)))
                        (M.lookup "allocated_bytes" s <&> read)
                        (M.lookup "max_live_bytes" s  <&> read)
