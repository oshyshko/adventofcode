module Y23.D06 where

import           Parser

-- Time:      7  15   30
-- Distance:  9  40  200
timesAndDistances :: Parser ([Int], [Int])
timesAndDistances =
    (,) <$> (string "Time: "    *> many1 (padded natural)) <* eol
        <*> (string "Distance:" *> many1 (padded natural)) <* eol

waysToWin :: Int -> Int -> Int
waysToWin t d =
    let sqDis :: Float = sqrt . fromIntegral $ t * t - 4 * d
        x1 = (fromIntegral t + sqDis) / 2
        x2 = (fromIntegral t - sqDis) / 2
    in 1 + (floor x1  - ceiling x2)

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
