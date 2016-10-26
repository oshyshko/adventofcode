module Main where

import qualified Day01
import qualified Day02
import qualified Day03
-- import qualified Day04
import qualified Day05
-- import qualified Day06
import qualified Day06Perf
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10

import Text.Printf (printf)

main :: IO ()
main = mapM_
    (\ (day, ioa) -> printf "Day%-10s: " day >> ioa)
    [  ("01",     Day01.main)
     , ("02",     Day02.main)
     , ("03",     Day03.main)
     , ("04 *",   putStrLn "[117946,3938038]")
     , ("05",     Day05.main)
    --  , ("06",     Day06.main)
     , ("06Perf", Day06Perf.main)
     , ("07",     Day07.main)
     , ("08",     Day08.main)
     , ("09",     Day09.main)
     , ("10",     Day10.main)
    ]
    >> putStrLn ""
    >> putStrLn "[*] replaced with a constant to save time"
