module Y15.D03 where

import qualified Data.Set as S

import           Imports
import           XY

char2move :: Char -> XY
char2move = \case
    '<' -> XY (-1)   0
    '^' -> XY   0  (-1)
    '>' -> XY   1    0
    'v' -> XY   0    1
    x   ->  error $ "Unexpected character: " ++ [x]

-- ^^<<v<<v><v^^<><>^^ ...
moves2houses :: String -> [XY]
moves2houses = scanl (+) 0 . map char2move

solve1 :: String -> Int
solve1 = S.size . S.fromList . moves2houses

solve2 :: String -> Int
solve2 xs =
    let (santaPairs, robotPairs) = partition (even . fst) $ zip [(0::Int)..] xs
        santaHouses              = moves2houses $ snd <$> santaPairs
        robotHouses              = moves2houses $ snd <$> robotPairs
    in S.size . S.fromList $ santaHouses ++ robotHouses
