module Day10 where

import           Control.Monad ((<=<))
import qualified Data.List     as L

solve :: String -> String
solve = concat . sequence [show . length, (:[]) . head] <=< L.group

main :: IO ()
main = do
  s <- head . lines <$> readFile "Day10.txt"
  print . sequence [ \xs -> length (iterate solve xs L.!! 40)
                   , \xs -> length (iterate solve xs L.!! 50)
                   ] $ s
