module Y21.D01 where

import Imports

solve1 :: String -> Int
solve1 =
      length
    . filter (\[a, b::Int] -> a < b)
    . divvy 2 1
    . map read
    . lines

solve2 :: String -> Int
solve2 =
      length
    . filter (\[a, b::Int] -> a < b)
    . divvy 2 1
    . map sum
    . divvy 3 1
    . map read
    . lines
