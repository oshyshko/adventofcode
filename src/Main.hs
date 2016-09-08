module Main where

import qualified Day01
import qualified Day02
import qualified Day03
-- import qualified Day04

import Control.Monad
import Text.Printf

main :: IO ()
main = zipWithM_
    (\ioa day -> printf "Day %02d: " (day::Int) >> ioa)
    [ Day01.main
    , Day02.main
    , Day03.main
    -- , Day04.main
    , putStrLn "(117946,3938038) <-- printing a constant to save time"
    ]
    [1..]
