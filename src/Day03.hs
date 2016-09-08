module Day03 where

import Control.Arrow ((&&&))
import Data.List     (nub, partition)

char2move :: Char -> (Int, Int)
char2move x = case x of '<' -> (-1,  0)
                        '^' -> ( 0, -1)
                        '>' -> ( 1,  0)
                        'v' -> ( 0,  1)
                        _   ->  error $ "Unexpected character: " ++ [x]

moves2houses :: String -> [(Int, Int)]
moves2houses = scanl (\(xa,ya) (x,y) -> (xa+x, ya+y)) (0, 0)
             . map char2move

solve1 :: String -> Int
solve1 = length . nub . moves2houses

solve2 :: String -> Int
solve2 xs = let (santaPairs, robotPairs) = partition (even . fst) $ zip [0..] xs
                santaHouses = moves2houses $ map snd santaPairs
                robotHouses = moves2houses $ map snd robotPairs
             in length . nub $ santaHouses ++ robotHouses

main :: IO ()
main = do
  xs <- readFile "Day03.txt"
  print . (solve1 &&& solve2) $ xs
