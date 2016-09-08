module Day05 where

import Data.List (isInfixOf)
import Util      (juxt)

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (drop 1 xs)

triplets :: [a] -> [(a,a,a)]
triplets xs = zip3 xs (drop 1 xs) (drop 2 xs)

isNice1 :: String -> Bool
isNice1 = and . juxt
           [ (>= 3) . length . filter (`elem` "aeiou") -- contains 3+ vowels
           , any (uncurry (==)) . pairs                -- contains 1+ symmetric pair. Note: "uncurry (==)" is equivalent to "(\(q,p) -> q == p)"
           , not . any (`elem` [ ('a','b')             -- does not contain these pairs: "ab", "cd", "pq", "xy"
                               , ('c','d')
                               , ('p','q')
                               , ('x','y') ]) . pairs ]

contains2EqPairs :: String -> Bool
contains2EqPairs (a:b:xs) = ([a,b] `isInfixOf` xs) || contains2EqPairs (b:xs)
contains2EqPairs _        = False

isNice2 :: String -> Bool
isNice2 = and . juxt
            [ contains2EqPairs                       -- contains 2 non-overlapping pairs
            , any (\ (q,_,p) -> q == p) . triplets ] -- contains one with equal neighbours

solve1 :: [String] -> Int
solve1 = length . filter isNice1

solve2 :: [String] -> Int
solve2 = length . filter isNice2

main :: IO ()
main = do
  xs <- readFile "Day05.txt"
  print . juxt [solve1, solve2] . lines $ xs
