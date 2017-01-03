module Main where

import           Control.DeepSeq       (force, NFData)
import           Data.Ratio            (numerator)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Text.Printf           (printf)

import qualified Y2015.D01
import qualified Y2015.D02
import qualified Y2015.D03
import qualified Y2015.D04
import qualified Y2015.D05
import qualified Y2015.D06
import qualified Y2015.D06Perf
import qualified Y2015.D07
import qualified Y2015.D08
import qualified Y2015.D09
import qualified Y2015.D10
import Control.Exception

-- $ ./scripts/build-exec.sh
-- + stack exec adventofcode-exe
-- Y2015.D01      --> [138,1771]           solved within [2,4] ms
-- Y2015.D02      --> [1586300,3737498]    solved within [42,12] ms
-- Y2015.D03      --> [2565,2639]          solved within [69,73] ms
-- Y2015.D04      --> [117946,3938038]     solved within [736,23965] ms
-- Y2015.D05      --> [236,51]             solved within [2,2] ms
-- Y2015.D06      --> [400410,15343601]    solved within [12732,14257] ms
-- Y2015.D06Perf  --> [400410,15343601]    solved within [322,326] ms
-- Y2015.D07      --> [3176,14710]         solved within [3,2] ms
-- Y2015.D08      --> [1333,2046]          solved within [2,3] ms
-- Y2015.D09      --> [117,909]            solved within [95,88] ms
-- Y2015.D10      --> [360154,5103798]     solved within [124,1815] ms

main :: IO ()
main = mapM_
    (\(file, title, solvers) ->
      do input <- readFile ("res/" ++ file ++ ".txt")
         at <- mapM (\solve -> timeOf (solve input)) solvers
         printf "%-14s --> %-20s solved within %s ms\n"
                title
                (show . map fst $ at)
                (show . map snd $ at))
    days

days :: [(String, String, [String -> IO Int])]
days = [ ("Y2015/D01", "Y2015.D01", map (return .) [Y2015.D01.solve1, Y2015.D01.solve2])
       , ("Y2015/D02", "Y2015.D02", map (return .) [Y2015.D02.solve1, Y2015.D02.solve2])
       , ("Y2015/D03", "Y2015.D03", map (return .) [Y2015.D03.solve1, Y2015.D03.solve2])
       , ("Y2015/D04", "Y2015.D04", map (return .) [Y2015.D04.solve1, Y2015.D04.solve2])
       , ("Y2015/D05", "Y2015.D05", map (return .) [Y2015.D05.solve1, Y2015.D05.solve2])
       , ("Y2015/D06", "Y2015.D06", map (return .) [Y2015.D06.solve1, Y2015.D06.solve2])
       , ("Y2015/D06", "Y2015.D06Perf",            [Y2015.D06Perf.solve1, Y2015.D06Perf.solve2])
       , ("Y2015/D07", "Y2015.D07", map (return .) [Y2015.D07.solve1, Y2015.D07.solve2])
       , ("Y2015/D08", "Y2015.D08", map (return .) [Y2015.D08.solve1, Y2015.D08.solve2])
       , ("Y2015/D09", "Y2015.D09", map (return .) [Y2015.D09.solve1, Y2015.D09.solve2])
       , ("Y2015/D10", "Y2015.D10", map (return .) [Y2015.D10.solve1, Y2015.D10.solve2])
       ]

timeOf :: NFData a => IO a -> IO (a, Integer)
timeOf ioa = do start <- timeInMillis
                a <- force <$> ioa
                evaluate a
                end <- timeInMillis
                return (a, end - start)
             where timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
                   timeInMillis = (`div` 1000) . fromIntegral <$> timeInMicros
