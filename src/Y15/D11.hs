module Y15.D11 where

import           Data.Char (isAsciiLower)

type ReversedStr = String

-- TODO optimize: skip invalid steps instead of generating all of them
isValid :: ReversedStr -> Bool
isValid s =
    not (containsIOL s)
    && containsPairs (2::Int) s
    && contains3consequent s
  where
      containsIOL = any (\x -> x == 'i' || x == 'o' || x == 'l')
      containsPairs n = \case
          (a:b:xs) -> if a == b
                          then n == 1 || containsPairs (n - 1) xs
                          else containsPairs n (b:xs)
          _        -> False
      contains3consequent = \case
          (a:b:c:xs) -> (a == succ b) && (b == succ c) || contains3consequent (b:c:xs)
          _          -> False

incChar :: Char -> Char
incChar x =
    if isAsciiLower x
        then if x == 'z' then 'a' else succ x
        else error $ "Invalid character in input: " ++ show x

incStr :: ReversedStr -> ReversedStr
incStr = \case
    []     -> []
    (x:xs) -> if x == 'z' then 'a' : incStr xs else incChar x : xs

solve1 :: String -> String
solve1 = reverse . head . filter isValid . tail . iterate incStr . reverse

solve2 :: String -> String
solve2 = solve1 . solve1
