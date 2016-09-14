module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06

import Control.Monad (zipWithM_)
import Text.Printf   (printf)

main :: IO ()
main = zipWithM_
    (\ioa day -> printf "Day %02d: " (day::Int) >> ioa)
    [ Day01.main
    , Day02.main
    , Day03.main
    , Day04.main
    , Day05.main
    , Day06.main
    ]
    [1..]
