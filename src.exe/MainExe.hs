module Main
    ( main
    ) where

import qualified Data.Map.Strict    as M
import           System.Environment (getArgs, getExecutablePath)
import           System.Exit        (ExitCode (..), exitFailure)
import           System.Process     (readProcessWithExitCode)

import qualified Days
import           Imports
import qualified Report
import qualified SysInfo
import           Types
import           Util

-- TODO refactor this mess + in DaysTH

-- # day     answer-1  answer-2
-- Y15.D01   138       1771
-- Y15.D02   1586300   3737498
{-# ANN parseAnswers ("HLint: ignore Use map once" :: String) #-}
parseAnswers :: String -> Map DayPrefix [String]
parseAnswers =
      M.fromListWithKey (\k _ _ -> error $ "Got a dupicate answer for: " ++ k)
    . map (\case
        day:answers -> (day, answers)
        x           -> error $ "Couldn't parse answers: " ++ show x)
    . map (filter (/= "") . splitOn " ")
    . filter (/= "")
    . filter (not . isPrefixOf "#")
    . lines

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- TODO refactor: encode part1or2 in a type?
        ["exec", moduleName, part1or2] -> readInput moduleName >>= runDay moduleName part1or2 >>= putStrLn
        ["pipe", moduleName, part1or2] -> getContents          >>= runDay moduleName part1or2 >>= putStr

        _ -> do
            -- select day(s)
            -- TODO refactor: use some args library?
            let daysPred = case args of
                    ["alts"]     -> const True
                    []           -> \Day{runAlts}           ->                             not runAlts
                    [x]          -> \Day{dayPrefix,runAlts} -> x `isPrefixOf` dayPrefix && not runAlts
                    [x, "alts"]  -> \Day{dayPrefix}         -> x `isPrefixOf` dayPrefix
                    _            -> error $ "Don't know how to interpret args: " ++ show args
                                        ++ "\nExamples:"
                                        ++ "\n                      ./scripts/build-exec.sh"
                                        ++ "\n                      ./scripts/build-exec.sh alts"
                                        ++ "\n                      ./scripts/build-exec.sh Y15"
                                        ++ "\n                      ./scripts/build-exec.sh Y15 alts"
                                        ++ "\n                      ./scripts/build-exec.sh exec Y15.D05 0"
                                        ++ "\n                      ./scripts/build-exec.sh exec Y15.D05 0 +RTS -t -s -RTS"
                                        ++ "\ncat res/Y15/D05.txt | ./scripts/build-exec.sh pipe Y15.D05 0"
                                        ++ "\ncat res/Y15/D05.txt | ./scripts/build-exec.sh pipe Y15.D05 0 +RTS -t -s -RTS"

            let daysSelected :: [Day] = filter daysPred $ M.elems Days.moduleName2day

            when (null daysSelected) $
                error $ "Couldn't find day " ++ (show . head) args
                    ++ ".\nAvailable days are: " ++ intercalate ", " (M.keys Days.moduleName2day) ++ ".\n"

            -- read answers.txt
            !mod2answers <- parseAnswers <$> readFile "res/answers.txt" -- NOTE err before printing the header

            Report.printHeader

            -- run
            dayPrefix2results <- forM daysSelected \Day{dayPrefix} -> do
                input <- readInput dayPrefix

                Report.printDayPrefix dayPrefix
                hFlush stdout

                results <- forM [1,2] $ \part1or2 -> do
                    pipeDay dayPrefix part1or2 input >>= \case
                        -- TODO 1. correctly present error (Left) and continue with the rest of days
                        -- TODO 2. find a better way to remove RTS part that starts with " [("
                        Left err -> error $
                                "solver for part "
                                ++ show part1or2 ++ " failed: "
                                ++ (unlines . takeWhile (\e -> not $ " [(" `isPrefixOf` e) . lines $ err)
                        Right r -> pure r

                Report.printDayResults mod2answers dayPrefix results

                pure (dayPrefix, results)

            Report.printFooter dayPrefix2results =<< SysInfo.getSysInfo

            let allAnswersCorrect = dayPrefix2results & all \(dayPrefix,results) ->
                    mod2answers M.!? Report.dayPrefixToModuleName dayPrefix -- expected
                        == Just (results <&> output)                        -- actual

            unless allAnswersCorrect exitFailure

runDay :: ModuleName -> String -> Input -> IO String
runDay moduleName part1or2 input =
    case M.lookup moduleName Days.moduleName2day of
        Nothing           -> error $ "Couldn't find day for prefix" ++ moduleName ++ ", solver for part " ++ part1or2
        Just Day{solvers} -> input & (solvers !! (read part1or2 - 1))

pipeDay :: DayPrefix -> Part1or2 -> Input -> IO (Either String RunResult)
pipeDay dayPrefix part1or2 input = do
    selfPath <- getExecutablePath
    (e, out, err) <- readProcessWithExitCode
        selfPath
        ["pipe", dayPrefix, show part1or2, "+RTS", "-t", "--machine-readable", "-RTS"]
        input

    case e of
        -- TODO move error to stdout? (to separate from stats in stderr)
        ExitFailure _ -> pure . Left $ err
        ExitSuccess -> do
            -- TODO handle parse error in stdout
            let s = M.fromList @String @String $ read err

            -- total_cpu_seconds, total_wall_seconds
            --   mut_cpu_seconds,   mut_wall_seconds
            pure . Right $ RunResult
                { output         = out
                , msReal         = ceiling @Double . (* 1000) . read $ s M.! "total_wall_seconds"
                , bytesAllocated = read $ s M.! "allocated_bytes"
                , bytesPeak      = read $ s M.! "max_live_bytes"
                , bytesMaxInUse  = read $ s M.! "max_mem_in_use_bytes"
                }
