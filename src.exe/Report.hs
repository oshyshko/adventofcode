module Report where

import qualified Data.Map.Strict as M
import           Imports
import           SysInfo         (SysInfo (..))
import           Util

type DayInput  = String
type DayKey    = String
type DayIndex  = Int

data ExecResult = ExecResult
    { output         :: String
    , msReal         :: Integer
    , bytesAllocated :: Maybe Integer
    , bytesPeak      :: Maybe Integer
    } deriving (Show)

header :: IO ()
header = do
    putStrLn "-----------+--------------------+- day 1 ---------------+- day 2 ---------------"
    putStrLn " day       | answers            |    time  alloc   peak |    time  alloc   peak"
    putStrLn "-----------+--------------------+-----------------------+-----------------------"

dayPrefix :: DayKey -> IO ()
dayPrefix dayKey = do
    printf " %-9s | " dayKey
    hFlush stdout

dayResults :: FilePath -> Map DayKey [String] -> DayKey -> [ExecResult] -> IO ()
dayResults answersPath day2answers dayKey results = do
    printf "%-18s | %s %s\n"
        (intercalate ", " $ results <&> output)
        (intercalate " | " $ results <&> \r ->
            printf "%5dms %6s %6s"
                (msReal r)
                (maybe "?" size2humanSize $ bytesAllocated r)
                (maybe "?" size2humanSize $ bytesPeak r))
        (case M.lookup (take (length ("YXX.DXX" :: String)) dayKey) day2answers of
            Nothing -> " <-- couldn't find entry " ++ show dayKey ++ " in " ++ show answersPath
            Just expected ->
                if expected /= (results <&> output) then
                    " <-- expected: " ++ intercalate ", " expected
                    else
                    "")

footer :: SysInfo -> IO ()
footer i = do
    putStrLn "-----------+--------------------+-----------------------+-----------------------"
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
