module Report where

import qualified Data.Map.Strict as M
import           Imports
import           SysInfo         (SysInfo (..))
import           Types
import           Util

header :: IO ()
header = do
    putStrLn "-----------+---------------------+- part 1 --------------+- part 2 --------------"
    putStrLn " day       | answers             |    time  alloc   peak |    time  alloc   peak"
    putStrLn "-----------+---------------------+-----------------------+-----------------------"

dayPrefix :: DayPrefix -> IO ()
dayPrefix dayP = do
    printf " %-9s | " dayP
    hFlush stdout

dayPrefixToModuleName :: DayPrefix -> ModuleName
dayPrefixToModuleName = take (length ("YXX.DXX" :: String))

dayResults :: FilePath -> Map ModuleName [AnswerStr] -> DayPrefix -> [ExecResult] -> IO ()
dayResults answersPath mod2answers dayP results = do
    printf "%-19s | %s %s\n"
        (intercalate ", " $ results <&> output)
        (intercalate " | " $ results <&> \r ->
            printf "%5dms %6s %6s"
                (msReal r)
                (maybe "?" size2humanSize $ bytesAllocated r)
                (maybe "?" size2humanSize $ bytesPeak r))
        (case M.lookup (dayPrefixToModuleName dayP) mod2answers of
            Nothing -> " <-- couldn't find entry " ++ show dayP ++ " in " ++ show answersPath
            Just expected ->
                if expected /= (results <&> output)
                    then " <-- expected: " ++ intercalate ", " expected
                    else "")

footer :: SysInfo -> IO ()
footer i = do
    putStrLn "-----------+---------------------+-----------------------+-----------------------"
    putStrLn . unlines . map (" " ++) . lines . showSysInfo $ i

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
                        , fm $ show <$> ramSpeed, "MHz"
        , "\nCompiler: ", fm compiler, " ("
                        , fm compilerArch, ")"
        ]
