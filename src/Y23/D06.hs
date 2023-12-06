module Y23.D06 where

import Parser
import Imports

-- Time:      7  15   30
-- Distance:  9  40  200
timesAndDistances :: Parser ([Int], [Int])
timesAndDistances =
    (,) <$> (string "Time: "    *> many1 (padded natural)) <* eol
        <*> (string "Distance:" *> many1 (padded natural)) <* eol

waysToWin :: Int -> Int -> Int
waysToWin t d =
      [0 :: Int .. t]
    & fmap (\x -> x * (t - x))
    & filter (>d)
    & length

solve1 :: String -> Int
solve1 =
      product
    . fmap (uncurry waysToWin)
    . uncurry zip
    . parseOrDie timesAndDistances

solve2 :: String -> Int
solve2 =
      (\(ts, ds) -> waysToWin (read $ show =<< ts) (read $ show =<< ds))
    . parseOrDie timesAndDistances
