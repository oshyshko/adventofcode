module Y15.D04 where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5       (md5)
import Data.List                  (isPrefixOf)

solve' :: Int -> String -> Int
solve' n s = fst
          . head
          . filter (isPrefixOf (replicate n '0') . snd)
          . map (\x -> (x, show . md5 . pack $ s ++ show x))
          $ [1..]

solve1 :: String -> Int
solve1 = solve' 5

solve2 :: String -> Int
solve2 = solve' 6
