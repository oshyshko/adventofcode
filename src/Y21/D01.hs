module Y21.D01 where

import           Imports
import           Util    (divvy2)

solve1 :: String -> Int
solve1 =
      length
    . filter (\(a, b::Int) -> a < b)
    . divvy2 1
    . map read
    . lines

solve2 :: String -> Int
solve2 =
      length
    . filter (\(a, b::Int) -> a < b)
    . divvy2 1
    . map sum
    . divvy 3 1
    . map read
    . lines
