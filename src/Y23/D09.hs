{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/fmap" #-}
module Y23.D09 where

import           Util (divvy2)

-- 0 3 6 9 12 15
-- 1 3 6 10 15 21
-- 10 13 16 21 30 45
parse :: String -> [[Int]]
parse = fmap (fmap read . words) . lines

-- 0   3   6   9  12  15   B
--   3   3   3   3   3   A
--     0   0   0   0   0
diffs :: [Int] -> [[Int]]
diffs =
      takeWhile (not . all (== 0))
    . iterate (fmap (uncurry subtract) . divvy2 1)

solve1, solve2 :: String -> Int
solve1 = sum . fmap (sum         . fmap last . diffs) . parse
solve2 = sum . fmap (foldr (-) 0 . fmap head . diffs) . parse
