module Y15.D20 where

import Math.NumberTheory.Primes (factorise, unPrime)

{-# INLINE divisorSum #-}
divisorSum :: Int -> Int
divisorSum i = product
    [ s | (unPrime -> p, n) <- factorise i
        , let s = sum [ p ^ x | x <- [0..n] ] ]

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
        [house
           | house <- [1..]
           , let presents = divisorSum house * 10
           , minPresents <= presents]

solve2 :: String -> Int
solve2 s =
    let minPresents = read s
    in head
        [house
           | house <- [1..]
           , let presents = sum (filter (house `div` 50 <) $ divisors house) * 11
           , minPresents <= presents]

-- TODO implement using a huge vector

{-

Part 1.

   1 2 3 4 5 6 7 8 9 A B C D E F G
 1 x                               |  1
 2 x x                             |  3
 3 x   x                           |  4
 4 x x   x                         |  7
 5 x       x                       |  6
 6 x x x     x                     | 12
 7 x           x                   |  8
 8 x x   x       x                 | 15
 9 x   x           x               | 13
10 x x     x         x             | 18
11 x                   x           | 12
12 x x x x   x           x         | 28
13 x                       x       | 14
14 x x         x             x     | 24
15 x   x   x                   x   | 24
16 x x   x       x               x | 31

> factorise 12
[(Prime 2,2),(Prime 3,1)]

2^0 * 3^0 = 1
2^0 * 3^1 = 3
2^1 * 3^0 = 2
2^1 * 3^1 = 6
2^2 * 3^0 = 4
2^2 * 3^1 = 12
1 + 3 + 2 + 6 + 4 + 12 = 28

2^[0..2] = 1 + 2 + 4
3^[0..1] = 1 + 3
(1 + 2 + 4) * (1 + 3) = 28


Part 2. For first 3 houses (part 2 requires 50).

   1 2 3 4 5 6 7 8 9 A B C D E F G
 1 x                               |  1
 2 x x                             |  3
 3 x   x                           |  4
 4   x   x                         |  6
 5         x                       |  5
 6   x x     x                     | 11
 7             x                   |  7
 8       x       x                 | 12
 9     x           x               | 12
10         x         x             | 15
11                     x           | 11
12       x   x           x         | 22
13                         x       | 13
14             x             x     | 21
15         x                   x   | 20
16               x               x | 24

-}
