module Day03 where

import Data.List (nub, partition)

char2move :: Char -> (Int, Int)
char2move x = case x of '<' -> (-1,  0)
                        '^' -> ( 0, -1)
                        '>' -> ( 1,  0)
                        'v' -> ( 0,  1)
                        _   ->  error $ "Unexpected character: " ++ [x]

moves2houses :: String -> [(Int, Int)]
moves2houses = scanl (\ (xa,ya) (x,y) -> (xa+x, ya+y) ) (0, 0)
             . map char2move

solve1 :: String -> Int
solve1 = length . nub . moves2houses

solve2 :: String -> Int
solve2 xs = let (santaPairs, robotPairs) = partition (even . fst) $ zip [0..] xs
                santaMoves = map snd santaPairs
                robotMoves = map snd robotPairs
             in length . nub $ moves2houses santaMoves ++ moves2houses robotMoves

main :: IO ()
main = do
  s <- readFile "Day03.txt"
  print $ solve1 s
  print $ solve2 s
