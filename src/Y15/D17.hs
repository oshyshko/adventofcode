module Y15.D17 where

parseContainers :: String -> [Int]
parseContainers = fmap read . lines

configs :: Int -> [Int] -> [[Int]]
configs 0 _      = [[]]
configs _ []     = []
configs n (x:xs) = ((x:) <$> configs (n-x) xs) <> configs n xs

solve1 :: String -> Int
solve1 = length . configs 150 . parseContainers

solve2 :: String -> Int
solve2 s =
    let cs = configs 150 $ parseContainers s
        m  = minimum $ fmap length cs
    in length $ filter ((== m) . length) cs
