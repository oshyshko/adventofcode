module Y21.D17 where

import           Data.Ord          (Down (..))

import           Imports
import           Parser
import           XY

-- target area: x=20..30, y=-10..-5
area :: Parser Line
area = do
    x0 <- string "target area: x=" *> integer
    x1 <- string ".."              *> integer
    y0 <- string ", y="            *> integer
    y1 <- string ".."              *> integer
    pure $ Line (XY x0 y0) (XY x1 y1)

-- (starting, iteration, value-at-hit, delta-remaining)
type SIVD = T4 Int Int Int Int

tick :: SIVD -> SIVD
tick (T4 s i v d) = T4 s (i + 1) (v + d) (d - 1)

genXs, genYs :: Int -> Int -> Int -> [SIVD]
genXs x0 x1 s =
    iterate tick (T4 s 0 0 s)
        & takeWhile (\(T4 _ _ _ d) -> d >=  0)  -- cut infinite fall
        & dropWhile (\(T4 _ _ x _) -> x <  x0)  -- cut left
        & takeWhile (\(T4 _ _ x _) -> x <= x1)  -- cut right
genYs y0 y1 s =
    iterate tick (T4 s 0 0 s)
        & dropWhile (\(T4 _ _ y _) -> y >  y1)  -- cut upper
        & takeWhile (\(T4 _ _ y _) -> y >= y0)  -- cut lower

solve :: String -> [XY]
solve s =
    let (Line (XY x0 y0) (XY x1 y1)) = parseOrDie area s
        xSteps = [1..x1]       >>= genXs x0 x1  -- NOTE if xSteps and ySteps inlined below,
        ySteps = [x0,x0-1..y0] >>= genYs y0 y1  --      allocs grow 9MB -> 800MB
    in  [ XY xs ys
        | (T4 xs xi _ xd) <- xSteps
        , (T4 ys yi _  _) <- ySteps
        , yi == xi || yi > xi && xd == 0
        ]

solve1, solve2 :: String -> Int
solve1 = (\(XY _ y) -> sum [0..y]) . head . sortOn (Down . getY) . solve
solve2 = length . nub . solve
