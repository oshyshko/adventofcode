module Y23.D02 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser

data Color = R | G | B deriving (Eq, Ord, Show)

data Game = Game
    { gameId :: Int
    , rounds :: [[(Color, Int)]]
    } deriving Show

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
-- Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
games :: Parser [Game]
games =
    game `endBy` eol
  where
    game = Game
        <$> (string "Game" *> padded natural <* char ':' )
        <*> ((color2count `sepBy` char ',') `sepBy` char ';')
    color2count = flip (,) <$> padded natural <*> color
    color = R <$ string "red"
        <|> G <$ string "green"
        <|> B <$ string "blue"

solve1 :: String -> Int
solve1 =
      sum
    . fmap gameId
    . filter (and . fmap hasEnough . concat . rounds)
    . parseOrDie games
  where
    -- 12 red cubes, 13 green cubes, and 14 blue cubes
    hasEnough :: (Color, Int) -> Bool
    hasEnough (c,n) = case c of
        R -> n <= 12
        G -> n <= 13
        B -> n <= 14

solve2 :: String -> Int
solve2 =
      sum
    . fmap (product . M.elems . M.fromListWith max . concat . rounds)
    . parseOrDie games
