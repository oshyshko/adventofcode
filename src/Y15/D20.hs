module Y15.D20 where

import           Math.NumberTheory.Primes (factorise, unPrime)

findFirstLE :: (Int -> Int) -> String -> Int
findFirstLE f s = head [ i | i <- [1..], read s <= f i ]

solve1 :: String -> Int
solve1 = findFirstLE \i -> 10 * divisorSum i
  where
     divisorSum i = product
        [ sum [ p ^ x | x <- [0..n] ]
        | (unPrime -> p, n) <- factorise i
        ]

solve2 :: String -> Int
solve2 = findFirstLE \i -> 11 * sum (filter (i `div` 50 <) $ divisors i)
  where
    divisors = foldr f [1] . factorise
    f (unPrime -> p, n) ys =
        [ y * p ^ x
        | x <- [0..n]
        , y <- ys
        ]
