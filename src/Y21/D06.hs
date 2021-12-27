module Y21.D06 where

import qualified Data.IntMap.Strict as M

import Imports

solve :: Int -> String -> Int
solve n =
      sum
    . map snd
    . (!! n)
    . iterate tick
    . map ((, 1::Int) . read @Int)
    . splitOn ","
  where
    tick :: (k ~ M.Key, Num v) => [(k, v)] -> [(k, v)]
    tick xs = M.fromListWith (+) xs & M.toList >>= \case
        (0, v) -> [(6, v), (8, v)]
        (k, v) -> [(k-1, v)]

solve1 :: String -> Int
solve1 = solve 80

solve2 :: String -> Int
solve2 = solve 256
