module Y2015.D10 where

import           Control.Monad ((<=<))
import qualified Data.List     as L

solve' :: String -> String
solve' = concat . sequence [show . length, (:[]) . head] <=< L.group

solve1 :: String -> Int
solve1 = length . (L.!! 40) . iterate solve' . head . lines

solve2 :: String -> Int
solve2 = length . (L.!! 50) . iterate solve' . head . lines
