module Report where

import qualified Data.Map.Strict as M
import           Imports
import           SysInfo         (SysInfo (..))
import           Types
import           Util

printHeader :: IO ()
printHeader = do
    putStrLn "-----------+--------------------------+- part 1 ---------------------+- part 2 ---------------------"
    putStrLn " day       | answers                  |    time allocs maxhea maxmem |    time allocs maxhea maxmem "
    putStrLn "-----------+--------------------------+------------------------------+------------------------------"

printDayPrefix :: DayPrefix -> IO ()
printDayPrefix = printf " %-9s | "

printDayResults :: Map ModuleName [AnswerStr] -> DayPrefix -> [RunResult] -> IO ()
printDayResults mod2answers dayPrefix results = do
    printf "%-24s |%s %s\n"
        (intercalate ", "  $ results <&> output)
        (intercalate " |" $ results <&> formatRunResult)
        (case M.lookup (dayPrefixToModuleName dayPrefix) mod2answers of
            Nothing -> " <-- couldn't find answer"
            Just expected ->
                if expected /= (results <&> output)
                    then " <-- expected: " ++ intercalate ", " expected
                    else "")

printFooter :: [(DayPrefix,[RunResult])] -> SysInfo -> IO ()
printFooter dayPrefix2results i = do
    let mergedResults = foldl1' (zipWith append) (snd <$> dayPrefix2results)

    putStrLn "-----------+--------------------------+------------------------------+------------------------------"
    printf " Total:                               |%s\n"
        (intercalate " |" $ mergedResults <&> formatRunResult)

    putStrLn ""
    putStrLn . unlines . map (" " ++) . lines . showSysInfo $ i
  where
    append :: RunResult -> RunResult -> RunResult
    append a b =
        RunResult
            { output         = ""
            , msReal         = msReal a         + msReal b
            , bytesAllocated = bytesAllocated a + bytesAllocated b
            , bytesPeak      = bytesPeak a      + bytesPeak b
            , bytesMaxInUse  = bytesMaxInUse a  + bytesMaxInUse b
            }

formatRunResult :: RunResult -> String
formatRunResult RunResult{msReal,bytesAllocated,bytesPeak,bytesMaxInUse} =
    printf "%6dms %6s %6s %6s"
        msReal
        (size2humanSize bytesAllocated)
        (size2humanSize bytesPeak)
        (size2humanSize bytesMaxInUse)

dayPrefixToModuleName :: DayPrefix -> ModuleName
dayPrefixToModuleName = take (length ("YXX.DXX" :: String))

showSysInfo :: SysInfo -> String
showSysInfo SysInfo{..} =
    let fm = fromMaybe "?"
    in intercalate ""
        [   "Platform: ", fm osName, ", "
                        , fm osArch, ", v"
                        , fm osVersion, ", "
                        , fm hwModel
        , "\nCPU:      ", fm cpuModel, ", "
                        , fm $ show <$> cpuCores, " cores"
        , "\nRAM:      ", fm $ size2humanSize . fromIntegral <$> ramTotal, " @ "
                        , fm $ show <$> ramClock, "MHz"
        , "\nCompiler: ", fm compiler, " ("
                        , fm compilerArch, ")"
        , "\nBinary:   ", fm $ show <$> binarySize, " bytes"
        ]
