module Y15.D04 where

import           Crypto.Hash.MD5       (hash)
import           Data.Bits             ((.&.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC

solve :: (B.ByteString -> Bool) -> String -> Int
solve startsWithZeros s =
    head $ filter
        (startsWithZeros . hash . (BC.pack s <>) . BC.pack . show)
        [1..]

solve1 :: String -> Int
solve1 = solve $ \bs ->
       0 == B.index bs 0
    && 0 == B.index bs 1
    && 0 == B.index bs 2 .&. 0xF0

solve2 :: String -> Int
solve2 = solve $ \bs ->
       0 == B.index bs 0
    && 0 == B.index bs 1
    && 0 == B.index bs 2
