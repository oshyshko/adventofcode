module Y15.D02 where

import Data.List.Split (splitOn)

parse :: String -> [(Int, Int, Int)]
parse = map whl . lines
          where whl s = let [w, h, l] = map read . splitOn "x" $ s
                         in (w, h, l)

solve1 :: String -> Int
solve1 = sum
       . map (\(w, h, l) -> let a = w * h
                                b = h * l
                                c = w * l
                            in (a + b + c) * 2 + minimum [a, b, c])
       . parse

solve2 :: String -> Int
solve2 = sum
       . map (\(w, h, l) -> (w*h*l) + 2 * minimum [w+h, h+l, w+l])
       . parse
