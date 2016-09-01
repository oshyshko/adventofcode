module Main where

import qualified Day01
import qualified Day02
import qualified Day03

import Control.Monad
import Text.Printf

main :: IO ()
main = zipWithM_
    (\ioa day -> printf "Day%0d: " (day::Int) >> ioa)
    [ Day01.main
    , Day02.main
    , Day03.main]
    [1..]
