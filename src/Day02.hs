module Day02 where

import Control.Arrow   ((&&&))
import Data.List.Split (splitOn)

solve1 :: [[Int]] -> Int
solve1 = sum . map (\[w,h,l] -> let a = w * h
                                    b = h * l
                                    c = w * l
                                 in (a+b+c) * 2 + minimum [a,b,c])

solve2 :: [[Int]] -> Int
solve2 = sum . map (\[w,h,l] -> (w*h*l) + 2 * minimum [w+h, h+l, w+l])

main :: IO ()
main = do
  s <- readFile "Day02.txt"
  let lineTo3Ints = map read . splitOn "x"
  print . (solve1 &&& solve2) . map lineTo3Ints . lines $ s
