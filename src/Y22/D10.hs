module Y22.D10 where

import           Imports
import qualified Letters as L
import           Parser

data Op = AddX Int | Noop

-- addx 1
-- noop
ops :: Parser [Op]
ops =
    op `endBy` eol
  where
    op =    (AddX <$> (string "addx" *> pad *> integer))
        <|> (Noop <$ string "noop")

xStates :: [Op] -> [Int]
xStates =
      scanl (+) 1
    . concatMap \case
        AddX a -> [0, a]
        Noop   -> [0]

solve1 :: String -> Int
solve1 =
      sum
    . (\cycles -> fmap (\c -> c * cycles !! (c-1)) [20,60..220])
    . xStates
    . parseOrDie ops

solve2 :: String -> String
solve2 =
    L.parse . renderCrt . xStates . parseOrDie ops
  where
    renderCrt =
          divvy 40 40
        . fmap (\(i,x) -> if abs (i - x - 1) < 2 then '#' else ' ')
        . zip (cycle [1..40])
