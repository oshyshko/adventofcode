module Main where

import           Text.Printf   (printf)

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

main :: IO ()
main = mapM_
    (\(file, day, solve) ->
      do input <- readFile ("res/" ++ file ++ ".txt")
         answers <- solve input
         printf "%-14s: %s\n" day (show answers))
    [ ("Y2015/D01", "Y2015.D01", return . Y2015.D01.solve)
    , ("Y2015/D02", "Y2015.D02", return . Y2015.D02.solve)
    , ("Y2015/D03", "Y2015.D03", return . Y2015.D03.solve)
    , ("Y2015/D04", "Y2015.D04", return . Y2015.D04.solve)
    , ("Y2015/D05", "Y2015.D05", return . Y2015.D05.solve)
    , ("Y2015/D06", "Y2015.D06", return . Y2015.D06.solve)
    , ("Y2015/D06", "Y2015.D06Perf",      Y2015.D06Perf.solve)
    , ("Y2015/D07", "Y2015.D07", return . map fromIntegral . Y2015.D07.solve)
    , ("Y2015/D08", "Y2015.D08", return . Y2015.D08.solve)
    , ("Y2015/D09", "Y2015.D09", return . Y2015.D09.solve)
    , ("Y2015/D10", "Y2015.D10", return . Y2015.D10.solve)
    ]
