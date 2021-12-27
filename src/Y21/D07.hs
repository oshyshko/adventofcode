module Y21.D07 where

import Imports

solve :: (Int -> Int) -> String -> Int
solve cost s =
    let xs        = map (read @Int) . splitOn "," $ s
        costAll i = sum $ map (cost . abs . (i -)) xs
    in minimum $ map costAll [minimum xs..maximum xs] -- TODO get min+max in one pass

solve1 :: String -> Int
solve1 = solve id

solve2 :: String -> Int
solve2 = solve \x -> x * (x + 1) `div` 2
