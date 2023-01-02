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
        -- TODO refactor: encode dayNs in a type?
        ["runday", moduleName, dayNs] ->
            case M.lookup moduleName Days.moduleName2day of
                Nothing           -> error $ "Couldn't find day for prefix" ++ moduleName ++ ", solver " ++ dayNs
                Just Day{solvers} ->
                    getContents >>= (solvers !! read dayNs) >>= putStr

        _ -> do
            -- select day(s)
            -- TODO refactor: use some args library?
            let daysPred = case args of
                    ["fast"]     -> (\Day{benchmark}           -> not benchmark)
                    []           -> const True
                    [x]          -> (\Day{dayPrefix}           -> x `isPrefixOf` dayPrefix)
                    [x, "fast"]  -> (\Day{dayPrefix,benchmark} -> x `isPrefixOf` dayPrefix && not benchmark)
                    _            -> error $ "Don't know how to interpret args: " ++ show args
                                        ++ "\nExamples:"
                                        ++ "\n./scripts/build-exec.sh"
                                        ++ "\n./scripts/build-exec.sh fast"
                                        ++ "\n./scripts/build-exec.sh Y15"
                                        ++ "\n./scripts/build-exec.sh Y15 fast"
                                        ++ "\ncat res/Y15/D05.txt | ./scripts/build-exec.sh runday Y15.D05 0"
                                        ++ "\ncat res/Y15/D05.txt | ./scripts/build-exec.sh runday Y15.D05 0 +RTS -t -s -RTS"

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

                results <- forM [0,1] $ \solverIndex -> do
                    runDay input dayPrefix solverIndex >>= \case
                        -- TODO 1. correctly present error (Left) and continue with the rest of days
                        -- TODO 2. find a better way to remove RTS part that starts with " [("
                        Left err -> error $
                                "solver with index "
                                ++ show solverIndex ++ " failed: "
                                ++ (unlines . takeWhile (\e -> not $ " [(" `isPrefixOf` e) . lines $ err)
                        Right r -> pure r

                Report.printDayResults mod2answers dayPrefix results

                pure (dayPrefix, results)

            Report.printFooter dayPrefix2results =<< SysInfo.getSysInfo

            let allAnswersCorrect = and $ dayPrefix2results <&> \(dayPrefix,results) ->
                    let expected = mod2answers M.!? Report.dayPrefixToModuleName dayPrefix
                        actual   = Just (results <&> output)
                    in expected == actual

            unless allAnswersCorrect exitFailure

runDay :: Input -> DayPrefix -> SolverIndex -> IO (Either String RunResult)
runDay input dayPrefix solverIndex = do
    selfPath <- getExecutablePath
    (e, out, err) <- readProcessWithExitCode
        selfPath
        ["runday", dayPrefix, show solverIndex, "+RTS", "-t", "--machine-readable", "-RTS"]
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
