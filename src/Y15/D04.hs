module Y15.D04 where

import           Data.Bits                  ((.&.))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Digest.Pure.MD5       (md5, md5DigestBytes)

solve :: (B.ByteString -> Bool) -> String -> Int
solve startsWithZeros s =
      head
    . filter (\x -> startsWithZeros . md5DigestBytes . md5 $ L.pack s <> L.pack (show x))
    $ [1..]

solve1 :: String -> Int
solve1 = solve
    (\bs -> 0 == B.index bs 0
         && 0 == B.index bs 1
         && 0 == B.index bs 2 .&. 0xF0)

solve2 :: String -> Int
solve2 = solve
    (\bs -> 0 == B.index bs 0
         && 0 == B.index bs 1
         && 0 == B.index bs 2)
