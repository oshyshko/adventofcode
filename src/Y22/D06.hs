module Y22.D06 where

import qualified Data.Set as S
import           Imports

solve :: Int -> String -> Int
solve n =
      maybe (error "No solution") (+ n)
    . findIndex ((== n) . S.size . S.fromList)
    . divvy n 1

solve1, solve2 :: String -> Int
solve1 = solve 4
solve2 = solve 14
