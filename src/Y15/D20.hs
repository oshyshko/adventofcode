module Y15.D20 where

import Math.NumberTheory.Primes (factorise, unPrime)

{-# INLINE divisorSum #-}
divisorSum :: Int -> Int
divisorSum i = product
    [ s
    | (unPrime -> p, n) <- factorise i
    , let s = sum [ p ^ x | x <- [0..n] ]
    ]

{-# INLINE divisors #-}
divisors :: Int -> [Int]
divisors =
    go [1] . factorise
  where
    go xs [] = xs
    go xs ((unPrime -> p, n):rest) =
        go [ x * p^nn | x <- xs, nn <- [0..n] ] rest

solve1 :: String -> Int
solve1 s =
    let minPresents = read s
    in head
        [ house
        | house <- [1..]
        , let presents = divisorSum house * 10
        , minPresents <= presents
        ]

solve2 :: String -> Int
solve2 s =
    let minPresents = read s
    in head
        [ house
        | house <- [1..]
        , let presents = sum (filter (house `div` 50 <) $ divisors house) * 11
        , minPresents <= presents
        ]
