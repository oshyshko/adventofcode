module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day06Perf

import Text.Printf (printf)

main :: IO ()
main = mapM_
    (\ (day, ioa) -> printf "Day%-10s: " day >> ioa)
    [ ("01",     Day01.main)
    , ("02",     Day02.main)
    , ("03",     Day03.main)
    , ("04 *",   putStrLn "[117946,3938038]")
    , ("05",     Day05.main)
    , ("06 *",   putStrLn "[400410,15343601]")
    , ("06Perf", Day06Perf.main)
    ]
    >> putStrLn ""
    >> putStrLn "[*] replaced with a constant to save time"
