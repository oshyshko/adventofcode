module Day05 where

import Data.List (groupBy, partition, sortBy)
import Util      (juxt)

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (drop 1 xs)

triplets :: [a] -> [(a,a,a)]
triplets xs = zip3 xs (drop 1 xs) (drop 2 xs)

isNice1 :: String -> Bool
isNice1 = and . juxt
           [ (>= 3) . length . fst . partition (`elem` "aeiou") -- contains 3+ vowels
           , any (uncurry (==)) . pairs                         -- contains 1+ symmetric pair. Note: "uncurry (==)" is equivalent to "(\(q,p) -> q == p)"
           , not . any (`elem` [ ('a','b')                      -- does not contain these pairs:
                               , ('c','d')                      -- ... "ab", "cd", "pq", "xy"
                               , ('p','q')
                               , ('x','y') ]) . pairs ]

farEnough :: [Int] -> Bool
farEnough ixs = case ixs of []    -> False
                            [_]   -> False
                            [a,b] -> b - a >= 2
                            _     -> True

contains2EqPairs :: String -> Bool
contains2EqPairs = any (farEnough .  map fst)
                 . groupBy (\(_,a) (_,b) -> a == b)
                 . sortBy  (\(_,a) (_,b) -> compare a b)
                 . zip [1..]
                 . pairs

isNice2 :: String -> Bool
isNice2 = and . juxt
            [ contains2EqPairs                       -- contain 2 non-overallping pairs
            , any (\ (q,_,p) -> q == p) . triplets ] -- contains one with equal neighbours

solve1 :: [String] -> Int
solve1 = length . filter isNice1

solve2 :: [String] -> Int
solve2 = length . filter isNice2

main :: IO ()
main = do
  xs <- readFile "Day05.txt"
  print . juxt [solve1, solve2] . lines $ xs
