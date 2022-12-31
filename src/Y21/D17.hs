module Y21.D17 where

import           Data.Ord          (Down (..))

import           Imports
import           Parser
import           XY

-- target area: x=20..30, y=-10..-5
area :: Parser (XY,XY)
area = do
    x <- string "target area: x=" *> integer
    a <- string ".."              *> integer
    y <- string ", y="            *> integer
    b <- string ".."              *> integer
    pure (XY x y, XY a b)

-- (starting, iteration, value-at-hit, delta-remaining)
type SIVD = T4 Int Int Int Int

tick :: SIVD -> SIVD
tick (T4 s i v d) = T4 s (i + 1) (v + d) (d - 1)

genXs, genYs :: Int -> Int -> Int -> [SIVD]
genXs x a s =
    iterate tick (T4 s 0 0 s)
        & takeWhile (\(T4 _ _ _  d) -> d >=  0) -- cut infinite fall
        & dropWhile (\(T4 _ _ x' _) -> x' <  x) -- cut left
        & takeWhile (\(T4 _ _ x' _) -> x' <= a) -- cut right
genYs y b s =
    iterate tick (T4 s 0 0 s)
        & dropWhile (\(T4 _ _ y' _) -> y' >  b) -- cut upper
        & takeWhile (\(T4 _ _ y' _) -> y' >= y) -- cut lower

solve :: String -> [XY]
solve s =
    let (XY x y, XY a b) = parseOrDie area s
        xSteps = [1..a]     >>= genXs x a       -- NOTE if xSteps and ySteps inlined below,
        ySteps = [x,x-1..y] >>= genYs y b       --      allocs grow 9MB -> 800MB
    in  [ XY xs ys
        | (T4 xs xi _ xd) <- xSteps
        , (T4 ys yi _  _) <- ySteps
        , yi == xi || yi > xi && xd == 0
        ]

solve1, solve2 :: String -> Int
solve1 = (\(XY _ y) -> sum [0..y]) . head . sortOn (Down . getY) . solve
solve2 = length . nub . solve
