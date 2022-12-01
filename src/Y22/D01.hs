module Y22.D01 where

import Imports

solve1 :: String -> Int
solve1 =
      maximum
    . fmap (sum . fmap read)
    . splitWhen null
    . lines

solve2 :: String -> Int
solve2 =
      sum
    . take 3
    . sortBy (flip compare)
    . fmap (sum . fmap read)
    . splitWhen null
    . lines
