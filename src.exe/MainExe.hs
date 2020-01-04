module Main where

import           Control.Monad      (forM, forM_, when)
import           Data.Functor       ((<&>))
import           Data.List          (intercalate, isPrefixOf)
import           Data.List.Split    (splitOn)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs, getExecutablePath)
import           System.IO          (hFlush, stdout)
import           System.Process     (readProcessWithExitCode)
import           Text.Printf        (printf)

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

days :: M.Map String [String -> IO String]
days = M.fromList
    [ ("Y15.D01",  i2ios   [Y15.D01.solve1,   Y15.D01.solve2])
    , ("Y15.D02",  i2ios   [Y15.D02.solve1,   Y15.D02.solve2])
    , ("Y15.D03",  i2ios   [Y15.D03.solve1,   Y15.D03.solve2])
    , ("Y15.D04",  i2ios   [Y15.D04.solve1,   Y15.D04.solve2])
    , ("Y15.D05",  i2ios   [Y15.D05.solve1,   Y15.D05.solve2])
    , ("Y15.D06AI",ioi2ios [Y15.D06.solve1AI, Y15.D06.solve2AI])
    , ("Y15.D06AS",i2ios   [Y15.D06.solve1AS, Y15.D06.solve2AS])
    , ("Y15.D06MH",i2ios   [Y15.D06.solve1MH, Y15.D06.solve2MH])
    , ("Y15.D06MI",i2ios   [Y15.D06.solve1MI, Y15.D06.solve2MI])
    , ("Y15.D06MS",i2ios   [Y15.D06.solve1MS, Y15.D06.solve2MS])
    , ("Y15.D06VI",ioi2ios [Y15.D06.solve1VI, Y15.D06.solve2VI])
    , ("Y15.D06VS",i2ios   [Y15.D06.solve1VS, Y15.D06.solve2VS])
    , ("Y15.D07",  i2ios   [Y15.D07.solve1,   Y15.D07.solve2])
    , ("Y15.D08",  i2ios   [Y15.D08.solve1,   Y15.D08.solve2])
    , ("Y15.D09",  i2ios   [Y15.D09.solve1,   Y15.D09.solve2])
    , ("Y15.D10",  i2ios   [Y15.D10.solve1,   Y15.D10.solve2])
    , ("Y15.D11",  s2ios   [Y15.D11.solve1,   Y15.D11.solve2])
    , ("Y15.D12",  i2ios   [Y15.D12.solve1,   Y15.D12.solve2])
    , ("Y15.D13",  i2ios   [Y15.D13.solve1,   Y15.D13.solve2])
    , ("Y15.D14",  i2ios   [Y15.D14.solve1,   Y15.D14.solve2])
    , ("Y15.D15",  i2ios   [Y15.D15.solve1,   Y15.D15.solve2])
    , ("Y15.D16",  i2ios   [Y15.D16.solve1,   Y15.D16.solve2])
    , ("Y15.D17",  i2ios   [Y15.D17.solve1,   Y15.D17.solve2])
    , ("Y15.D18IO",ioi2ios [Y15.D18.solve1IO, Y15.D18.solve2IO])
    , ("Y15.D18ST",i2ios   [Y15.D18.solve1ST, Y15.D18.solve2ST])
    ]
  where s2ios :: [a -> b] -> [a -> IO b]
        s2ios   = fmap (return .)
        i2ios :: Show b => [a -> b] -> [a -> IO String]
        i2ios   = fmap ((return . show) .)
        ioi2ios :: Show b => [a -> IO b] -> [a -> IO String]
        ioi2ios = fmap (fmap show .)

-- # day     answer-1  answer-2
-- Y15.D01   138       1771
-- Y15.D02   1586300   3737498
{-# ANN parseAnswers ("HLint: ignore Use map once" :: String) #-}
parseAnswers :: String -> M.Map String [String]
parseAnswers =
    M.fromList
    . map (\case
            day:answers -> (day, answers)
            x -> error $ "Couldn't parse answers: " ++ show x)
    . map (filter (/= "") . splitOn " ")
    . filter (not . isPrefixOf "#")
    . lines

main :: IO ()
main = do
    args <- getArgs

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

            -- header
            putStrLn "-----------+--------------------+- day 1 ---------------+- day 2 ---------------"
            putStrLn " day       | answers            |    time  alloc   peak |    time  alloc   peak"
            putStrLn "-----------+--------------------+-----------------------+-----------------------"

            -- read answers.txt
            let answersPath = "res/answers.txt"
            day2answers <- parseAnswers <$> readFile answersPath

            -- run
            forM_ daysSelected
                (\ (dayKey, solvers) -> do
                    input <- readInput dayKey
                    printf " %-9s | " dayKey
                    hFlush stdout

                    results <- forM (take (length solvers) [0..]) $ runDayViaExec input dayKey

                    printf "%-18s | %s %s\n"
                        (intercalate ", " $ results <&> output)
                        (intercalate " | " $ results <&> \r ->
                            printf "%5dms %6s %6s"
                                (msReal r)
                                (size2humanSize . bytesAllocated $ r)
                                (size2humanSize . bytesPeak $ r))
                        (case M.lookup (take 7 dayKey) day2answers of
                            Nothing -> " <-- couldn't find entry " ++ show dayKey ++ " in " ++ show answersPath
                            Just expected ->
                                if expected /= (results <&> output)
                                then " <-- got wrong answers, expected: " ++ intercalate ", " expected
                                    ++ ", but got: " ++ intercalate ", " (output <$> results)
                                else ""))

            -- footer
            putStrLn "-----------+--------------------+-----------------------+-----------------------"
            -- TODO check exit code
            -- TODO check if windows -> use different command
            (_, out, _) <- readProcessWithExitCode "uname" ["-s", "-m", "-p"] ""
            putStrLn $ " " ++ out



data ExecResult = ExecResult
    { output         :: String
    , msReal         :: Integer
    , bytesAllocated :: Integer
    , bytesPeak      :: Integer
    } deriving (Show)

-- TODO report failures
runDayViaDirectCall :: String -> String -> Int -> IO ExecResult
runDayViaDirectCall input dayKey dayN = do
    let ioa = fromMaybe (error $ "Couldn't find day: " ++ dayKey) (M.lookup dayKey days) !! dayN
    (out, ms) <- timeOf $ ioa input
    return $ ExecResult out ms 0 0

runDayViaExec :: String -> String -> Int -> IO ExecResult
runDayViaExec input dayKey dayN  = do
    selfPath <- getExecutablePath

    (_, out, err) <- readProcessWithExitCode
        selfPath
        ["runday", dayKey, show dayN, "+RTS", "-t", "--machine-readable", "-RTS"]
        input

    -- TODO assert exit code
    -- TODO parse err

    let s = M.fromList $ read err

    -- total_cpu_seconds, total_wall_seconds
    --   mut_cpu_seconds,   mut_wall_seconds
    return $ ExecResult out
        (fromMaybe (-1) $ ceiling . (* 1000)
                      <$> (read :: String -> Double) <$> M.lookup ("total_wall_seconds"::String) s)
        (fromMaybe (-1) $ read <$> M.lookup "allocated_bytes" s)
        (fromMaybe (-1) $ read <$> M.lookup "max_live_bytes" s )
