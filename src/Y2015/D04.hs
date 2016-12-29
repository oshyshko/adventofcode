module Y2015.D04 where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5       (md5)
import Data.List                  (isPrefixOf)

solve' :: Int -> String -> Int
solve' n s = fst
          . head
          . filter (isPrefixOf (replicate n '0') . snd)
          . map (\x -> (x, show . md5 . pack $ s ++ show x))
          $ [1..]

solve :: String -> [Int]
solve = sequence [solve' 5, solve' 6]
