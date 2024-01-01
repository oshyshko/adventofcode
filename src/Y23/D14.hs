module Y23.D14 where

import           Imports
import           Util

-- ..#.
-- O.OO
-- .#..
-- OO.#
type Image = [String]

-- move "O.#..O.#.#" => ".O#...O#.#"
moveE1 :: String -> String
moveE1 =
    go (0,0)
  where
    go :: (Int, Int) -> String -> String
    go oe    ('#':xs) = flush oe ++ "#" ++ go (  0,   0) xs
    go (o,e) ('O':xs) =                    go (o+1, e  ) xs
    go (o,e) ('.':xs) =                    go (o,   e+1) xs
    go oe    []       = flush oe
    go _     _        = shouldNeverReachHere
    flush (o,e) = replicate e '.' ++ replicate o 'O'

moveN, moveW, moveS, moveE :: Image -> Image
moveN = transpose . fmap (reverse . moveE1 . reverse) . transpose
moveW =             fmap (reverse . moveE1 . reverse)
moveS = transpose . fmap            moveE1            . transpose
moveE =             fmap            moveE1

countWeight :: Image -> Int
countWeight =
      sum
    . fmap (\(n,s) -> n * countElem 'O' s)
    . zip [1..]
    . reverse

-- findPrefixAndRepeat $ [1..5] ++ cycle [6..10]  => ([1,2,3,4,5],[6,7,8,9,10])
findPrefixAndRepeat :: Eq a => [a] -> ([a], [a])
findPrefixAndRepeat =
      fmap fromJust
    . head . dropWhile (isNothing . snd)
    . flip scanl ([], Nothing) \(as,_) x ->
          x `elemIndex` as
        & maybe (x:as, Nothing) \i ->
            let rs = drop (length as - i - 1) (reverse as)
            in (reverse $ drop (length rs) as, Just rs)

mkNth :: ([a], [a]) -> Int -> a
mkNth (prefixPart,repeatPart) i =
    if i < length prefixPart
        then prefixPart !! i
        else repeatPart !! ((i - length prefixPart) `rem` length repeatPart)

solve1, solve2 :: String -> Int
solve1 = countWeight . moveN . lines
solve2 =
      countWeight
    . flip mkNth 1_000_000_000
    . findPrefixAndRepeat
    . iterate (moveE . moveS . moveW . moveN)
    . lines
