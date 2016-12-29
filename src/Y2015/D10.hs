module Y2015.D10 where

import           Control.Monad ((<=<))
import qualified Data.List     as L

solve' :: String -> String
solve' = concat . sequence [show . length, (:[]) . head] <=< L.group

solve :: String -> [Int]
solve s = sequence [ length . (L.!! 40)
                   , length . (L.!! 50)
                   ] $ iterate solve' . head $ lines s
