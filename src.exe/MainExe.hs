module Main
    ( main, mainArgs
    ) where

import qualified Data.Map.Strict    as M
import           System.Environment (getArgs, getExecutablePath)
import           System.Exit        (ExitCode (..))
import           System.Process     (readProcessWithExitCode)

import qualified Days
import           Imports
import qualified Report
import qualified SysInfo
import           Util
import           Types

-- # day     answer-1  answer-2
-- Y15.D01   138       1771
-- Y15.D02   1586300   3737498
{-# ANN parseAnswers ("HLint: ignore Use map once" :: String) #-}
parseAnswers :: String -> Map DayPrefix [String]
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
        -- TODO refactor: encode dayNs in a type?
        ["runday", moduleName, dayNs] ->
            case M.lookup moduleName Days.moduleName2day of
                Nothing           -> error $ "Couldn't find day for prefix" ++ moduleName ++ ", solver " ++ dayNs
                Just Day{solvers} -> getContents >>= (solvers !! read dayNs) >>= putStr

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
            let answersPath = "res/answers.txt"
            mod2answers <- parseAnswers <$> readFile answersPath

            Report.header

            -- run
            forM_ daysSelected $ \Day{dayPrefix,solvers} -> do
                input <- readInput dayPrefix

                Report.dayPrefix dayPrefix

                results <- forM (take (length solvers) [0..]) $ \solverIndex -> do
                    runDayViaExec input dayPrefix solverIndex >>= \case
                        -- TODO 1. correctly present error (Left) and continue with the rest of days
                        -- TODO 2. find a better way to remove RTS part that starts with " [("
                        Left err -> error $
                                "solver with index "
                                ++ show solverIndex ++ " failed: "
                                ++ (unlines . takeWhile (\e -> not $ " [(" `isPrefixOf` e) . lines $ err)
                        Right r -> return r

                Report.dayResults answersPath mod2answers dayPrefix results

            Report.footer =<< SysInfo.getSysInfo

-- TODO report failures
runDayViaDirectCall :: Input -> DayPrefix -> SolverIndex -> IO (Either String ExecResult)
runDayViaDirectCall input dayPrefix solverIndex = do
    let Day{solvers} =  fromMaybe
            (error $ "Couldn't find day: " ++ dayPrefix)
            (M.lookup dayPrefix Days.moduleName2day)

    (out, ms) <- timeOf $ (solvers !! solverIndex) input
    return . Right $ ExecResult
        { output         = out
        , msReal         = ms
        , bytesAllocated = Nothing
        , bytesPeak      = Nothing
        , bytesMaxInUse  = Nothing
        }

runDayViaExec :: Input -> DayPrefix -> SolverIndex -> IO (Either String ExecResult)
runDayViaExec input dayPrefix solverIndex = do
    selfPath <- getExecutablePath

    if "ghc" `isSuffixOf` selfPath
        -- fallback to direct call if we are in a REPL
        then runDayViaDirectCall input dayPrefix solverIndex
        else do
            (e, out, err) <- readProcessWithExitCode
                selfPath
                ["runday", dayPrefix, show solverIndex, "+RTS", "-t", "--machine-readable", "-RTS"]
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
                        { output         = out
                        , msReal         = maybe (-1)
                                                (ceiling . (* 1000))
                                                (M.lookup "total_wall_seconds" s
                                                    <&> (read :: String -> Double))
                        , bytesAllocated = M.lookup "allocated_bytes" s <&> read
                        , bytesPeak      = M.lookup "max_live_bytes" s  <&> read
                        , bytesMaxInUse  = M.lookup "max_mem_in_use_bytes" s  <&> read
                        }
