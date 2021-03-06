module Y15.D05 where

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (drop 1 xs)

triplets :: [a] -> [(a,a,a)]
triplets xs = zip3 xs (drop 1 xs) (drop 2 xs)

-- uxcplgxnkwbdwhrp
-- suerykeptdsutidb
isNice1 :: String -> Bool
isNice1 =
    and . sequence
        -- contains 3+ vowels
        [ (>= 3) . length . filter (`elem` ("aeiou"::String))
        -- contains 1+ symmetric pair. Note: "uncurry (==)" is equivalent to "(\(q,p) -> q == p)"
        , any (uncurry (==)) . pairs
        -- does not contain these pairs: "ab", "cd", "pq", "xy"
        , not . any (`elem`
            [ ('a','b')
            , ('c','d')
            , ('p','q')
            , ('x','y')
            ]) . pairs
        ]

contains2NonConsPairs :: [(Char,Char)] -> Bool
contains2NonConsPairs  = \case
    (a:bs@(_:cs)) -> a `elem` cs || contains2NonConsPairs bs
    _             -> False

isNice2 :: String -> Bool
isNice2 = and . sequence
    [ contains2NonConsPairs . pairs          -- contains 2 non-consequent pairs
    , any (\ (q,_,p) -> q == p) . triplets ] -- contains one with equal neighbours

solve1 :: String -> Int
solve1 = length . filter isNice1 . lines

solve2 :: String -> Int
solve2 = length . filter isNice2 . lines
