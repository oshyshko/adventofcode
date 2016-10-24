module Day10 where

import           Control.Monad ((<=<))
import qualified Data.List     as L

solve :: String -> String
solve = concat . sequence [show . length, (:[]) . head] <=< L.group

main :: IO ()
main = do
  s <- head . lines <$> readFile "Day10.txt"
  print . sequence [ length . (L.!! 40)
                   , length . (L.!! 50)
                   ] $ iterate solve s
