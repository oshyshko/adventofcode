module Y22.D10 where

import           Imports
import qualified Letters as L
import           Parser

data Op     = AddX Int | Noop  deriving Show

-- addx 1
-- noop
ops :: Parser [Op]
ops =
    op `endBy` eol
  where
    op =    (AddX <$> (string "addx" *> pad *> integer))
        <|> (Noop <$ string "noop")

states :: [Op] -> [Int]
states =
      concatMap snd
    . scanl (\(x, _) -> \case
        AddX a -> let x' = x + a in (x', [x, x'])
        Noop   -> (x, [x]))
        (1, [1])

solve1 :: String -> Int
solve1 =
      sum
    . (\cycles -> fmap (\c -> c * cycles !! (c-1)) [20,60..220])
    . states
    . parseOrDie ops

solve2 :: String -> String
solve2 =
      L.parse
    . renderCrt
    . states
    . parseOrDie ops
  where
    renderCrt =
          divvy 40 40
        . fmap (\(i,x) -> if abs (i - x - 1) < 2 then '#' else ' ')
        . zip (cycle [1..40])
