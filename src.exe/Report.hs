module Report where

import qualified Data.Map.Strict as M
import           Imports
import           SysInfo         (SysInfo (..))
import           Types
import           Util

printHeader :: IO ()
printHeader = do
    putStrLn "-----------+-----------------------+- part 1 ---------------------+- part 2 ---------------------"
    putStrLn " day       | answers               |    time allocs maxhea maxmem |    time allocs maxhea maxmem "
    putStrLn "-----------+-----------------------+------------------------------+------------------------------"

printFooter :: SysInfo -> IO ()
printFooter i = do
    putStrLn "-----------+-----------------------+------------------------------+------------------------------"
    putStrLn . unlines . map (" " ++) . lines . showSysInfo $ i

printDayPrefix :: DayPrefix -> IO ()
printDayPrefix dayPrefix = do
    printf " %-9s | " dayPrefix
    hFlush stdout

printDayResults :: FilePath -> Map ModuleName [AnswerStr] -> DayPrefix -> [ExecResult] -> IO ()
printDayResults answersPath mod2answers dayPrefix results = do
    printf "%-21s | %s %s\n"
        (intercalate ", " $ results <&> output)
        (intercalate " | " $ results <&> \ExecResult{msReal, bytesAllocated, bytesPeak, bytesMaxInUse} ->
            printf "%5dms %6s %6s %6s"
                msReal
                (maybe "?" size2humanSize bytesAllocated)
                (maybe "?" size2humanSize bytesPeak)
                (maybe "?" size2humanSize bytesMaxInUse))
        (case M.lookup (dayPrefixToModuleName dayPrefix) mod2answers of
            Nothing -> " <-- couldn't find entry " ++ show dayPrefix ++ " in " ++ show answersPath
            Just expected ->
                if expected /= (results <&> output)
                    then " <-- expected: " ++ intercalate ", " expected
                    else "")

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
        ]
