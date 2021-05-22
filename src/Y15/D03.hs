module Y15.D03 where

import           Data.List (nub, partition)

char2move :: Char -> (Int, Int)
char2move = \case
    '<' -> (-1,  0)
    '^' -> ( 0, -1)
    '>' -> ( 1,  0)
    'v' -> ( 0,  1)
    x   ->  error $ "Unexpected character: " ++ [x]

-- ^^<<v<<v><v^^<><>^^ ...
moves2houses :: String -> [(Int, Int)]
moves2houses =
    scanl (\(xa,ya) (x,y) -> (xa+x, ya+y)) (0, 0)
    . map char2move

solve1 :: String -> Int
solve1 = length . nub . moves2houses

solve2 :: String -> Int
solve2 xs =
    let (santaPairs, robotPairs) = partition (even . fst) $ zip [(0::Int)..] xs
        santaHouses              = moves2houses $ snd <$> santaPairs
        robotHouses              = moves2houses $ snd <$> robotPairs
    in length . nub $ santaHouses ++ robotHouses
