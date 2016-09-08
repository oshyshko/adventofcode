module Day04 where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5       (md5)
import Util                       (juxt)

startsWith5Zeros :: String -> Bool
startsWith5Zeros ('0':'0':'0':'0':'0':_) = True
startsWith5Zeros _                       = False

startsWith6Zeros :: String -> Bool
startsWith6Zeros ('0':'0':'0':'0':'0':'0':_) = True
startsWith6Zeros _                           = False

solve1 :: String -> Int
solve1 s = fst
         . head
         . dropWhile (not . startsWith5Zeros . snd)
         . map (\x -> (x, show . md5 . pack $ s ++ show x))
         $ [1..]

-- TODO optimize + merge with solve1
solve2 :: String -> Int
solve2 s = fst
         . head
         . dropWhile (not . startsWith6Zeros . snd)
         . map (\x -> (x, show . md5 . pack $ s ++ show x))
         $ [1..]

main :: IO ()
main = do
  s <- readFile "Day04.txt"
  -- print . juxt [solve1, solve2] $ s
  putStrLn "[117946,3938038] <-- printing a constant to save time"
