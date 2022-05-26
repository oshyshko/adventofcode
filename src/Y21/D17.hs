module Y21.D17 where

import           Imports
import           Util

-- target area: x=20..30, y=-10..-5
area :: Parser ((Int, Int), (Int, Int))
area =
    (,) <$> ((,) <$> (string "target area: x=" *> integer) <* string ".." <*> integer)
        <*> ((,) <$> (string            ", y=" *> integer) <* string ".." <*> integer)

-- (starting, iteration, value-at-hit, delta-remaining)
tick :: a ~ Int => (a, a, a, a) -> (a, a, a, a)
tick (s, i, v, d) = (s, i + 1, v + d, d - 1)

genXs, genYs :: a ~ Int => a -> a -> a -> [(a, a, a, a)]
genXs x0 x1 s =
    iterate tick (s, 0, 0, s)
        & takeWhile (\(_,_,_,d) -> d >=  0)
        & dropWhile (\(_,_,x,_) -> x <  x0)
        & takeWhile (\(_,_,x,_) -> x <= x1)
genYs y0 y1 s =
    iterate tick (s, 0, 0, s)
        & dropWhile (\(_,_,y,_) -> y >  y1)
        & takeWhile (\(_,_,y,_) -> y >= y0)

solve :: String -> [(Int, Int)]
solve s =
    let ((x0,x1), (y0,y1)) = parseOrDie area s
    in  [ (xs, ys)
        | (xs, xi, _, xd) <- [0..x1]       >>= genXs x0 x1  -- xs
        , (ys, yi, _,  _) <- [x0,x0-1..y0] >>= genYs y0 y1  -- ys
        , yi == xi || yi > xi && xd == 0
        ]

solve1, solve2 :: String -> Int
solve1 = (\(_,y) -> sum [0..y]) . head . sortOn (negate . snd) . solve
solve2 = length . nub . solve
