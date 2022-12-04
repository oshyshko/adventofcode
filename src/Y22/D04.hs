module Y22.D04 where

import           Parser

teams :: Parser [((Int, Int), (Int, Int))]
teams =
    pair `endBy` eol
  where
    pair  = (,) <$> range   <* char ',' <*> range
    range = (,) <$> natural <* char '-' <*> natural

solve1 :: String -> Int
solve1 =
    length . filter eitherWithin . parseOrDie teams
  where
    eitherWithin ((a,b), (x,y)) =
        a <= x && y <= b || x <= a && b <= y    -- a(xy)b, x(ab)y

solve2 :: String -> Int
solve2 =
    length . filter overlap . parseOrDie teams
  where
    overlap ((a,b), (x,y)) =
           (a <= x || b <= y) && x <= b         -- a(x)b, x(b)y
        || (x <= a || y <= b) && a <= y         -- a(y)b, x(a)y
