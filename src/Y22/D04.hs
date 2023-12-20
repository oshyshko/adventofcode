module Y22.D04 where

import           Geom.Range
import           Parser

teams :: Parser [(Range Int Int, Range Int Int)]
teams =
    pair `endBy` eol
  where
    pair = (,) <$> range <* char ',' <*> range
    range  = do
        o1 <- natural <* char '-'
        o2 <- natural
        -- covert (offset,offset) to (offset,size)
        pure $ Range o1 (o2 - o1 + 1)

solve1, solve2 :: String -> Int
solve1 = length . filter (\(a, b) -> a `contains` b || b `contains` a) . parseOrDie teams
solve2 = length . filter (uncurry intersects)                          . parseOrDie teams
