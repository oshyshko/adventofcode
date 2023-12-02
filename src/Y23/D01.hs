module Y23.D01 where

import           Imports

-- "1abc2"        -> 12
-- "pqr3stu8vwx"  -> 38
readFirstAndLastDigit :: String -> Int
readFirstAndLastDigit = read . (\s -> [head s, last s]) . filter isDigit

-- "two1nine"     -> "2wo19ine"
-- "eightwothree" -> "8igh2wo3hree"
-- "eighthree"    -> "8igh3hree"    (sliding window)
-- "sevenine"     -> "7eve9ine"     (sliding window)
replaceOneToNineNumeralsWithDigits :: String -> String
replaceOneToNineNumeralsWithDigits =
    concatMap tryReplace . takeWhile (not . null) . iterate tail
  where
    tryReplace :: String -> String
    tryReplace s
        | "one"     `isPrefixOf` s  = "1"
        | "two"     `isPrefixOf` s  = "2"
        | "three"   `isPrefixOf` s  = "3"
        | "four"    `isPrefixOf` s  = "4"
        | "five"    `isPrefixOf` s  = "5"
        | "six"     `isPrefixOf` s  = "6"
        | "seven"   `isPrefixOf` s  = "7"
        | "eight"   `isPrefixOf` s  = "8"
        | "nine"    `isPrefixOf` s  = "9"
        | otherwise                 = [head s]

solve1, solve2 :: String -> Int
solve1 = sum . fmap readFirstAndLastDigit . lines
solve2 = sum . fmap readFirstAndLastDigit . lines . replaceOneToNineNumeralsWithDigits
