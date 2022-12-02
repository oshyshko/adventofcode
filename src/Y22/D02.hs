module Y22.D02 where

import           Imports
import           Parser

data Move    = R | P | S deriving (Enum, Bounded)   -- Rock Paper Scissors
data Outcome = L | D | W deriving (Enum, Bounded)   -- Lost Draw Won

-- parsers
rounds :: Parser abc -> Parser xyz -> Parser [(abc, xyz)]
rounds parseAbc parseXyz =
    round' `endBy` eol
  where
    round' = (,) <$> parseAbc <* pad <*> parseXyz

parseMap :: forall a . Char -> Char -> Char -> a -> a -> a -> Parser a
parseMap c1 c2 c3 v1 v2 v3 =
        try (char c1) $> v1
    <|> try (char c2) $> v2
    <|> try (char c3) $> v3

-- table fns
atYX :: forall a b c. (Enum a, Enum b, Bounded a) => [c] -> a -> b -> c
atYX table y x = table !! ((fromEnum y * (fromEnum (maxBound :: a) + 1) ) + fromEnum x)

outcome :: Move -> Move -> Outcome
outcome = atYX
    --    R  P  S
        [ D, W, L   -- R
        , L, D, W   -- P
        , W, L, D   -- S
        ]

moveForOutcome :: Outcome -> Move -> Move
moveForOutcome = atYX
    --    L  D  W
        [ S, R, P   -- R
        , R, P, S   -- P
        , P, S, R   -- S
        ]

scoreMove :: Move -> Int
scoreMove = ([1, 2, 3] !!) . fromEnum

scoreOutcome :: Outcome -> Int
scoreOutcome = ([0, 3, 6] !!) . fromEnum

solve1 :: String -> Int
solve1 =
      sum
    . fmap (\(abc, xyz) -> scoreOutcome (outcome abc xyz) + scoreMove xyz)
    . parseOrDie (rounds
        (parseMap 'A' 'B' 'C' R P S)
        (parseMap 'X' 'Y' 'Z' R P S) )

solve2 :: String -> Int
solve2 =
      sum
    . fmap (\(abc, xyz) -> scoreOutcome xyz + scoreMove (moveForOutcome xyz abc))
    . parseOrDie (rounds
        (parseMap 'A' 'B' 'C' R P S)
        (parseMap 'X' 'Y' 'Z' L D W) )
