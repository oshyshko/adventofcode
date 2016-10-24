module Day10 where

import qualified Data.List as L

solve :: String -> String
solve s = concat . concat $ [ [show . length $ x, [head x]] | x <- L.group s]

main :: IO ()
main = do
  s <- head . lines <$> readFile "Day10.txt"
  print . sequence [ \xs -> length (iterate solve xs L.!! 40)
                   , \xs -> length (iterate solve xs L.!! 50)
                   ] $ s
