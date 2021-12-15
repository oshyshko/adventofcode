module Y21.D02 where

import           Imports
import           Util

data Op          = Down | Up | Forward                    deriving Show
data Command     = Command     { op :: Op, value :: Int } deriving Show
data PosDepth    = PosDepth    { pos, depth :: Int }      deriving Show
data PosDepthAim = PosDepthAim { pos, depth, aim :: Int } deriving Show

-- down 5
-- up 3
-- forward 5
commands :: Parser [Command]
commands =
    command `endBy` eol
  where
    command :: Parser Command
    command = Command <$> op <* space <*> decimal

    op :: Parser Op
    op =    try (string "forward") $> Forward
        <|> try (string "down")    $> Down
        <|>      string "up"       $> Up

solve1 :: String -> Int
solve1 =
      (\(PosDepth p d) -> p * d)
    . foldl' move (PosDepth 0 0)
    . parseOrDie commands
  where
    move :: PosDepth -> Command -> PosDepth
    move pd@(PosDepth p d) (Command op v) = case op of
        Down    -> pd {depth = d + v}
        Up      -> pd {depth = d - v}
        Forward -> pd {pos   = p + v}

solve2 :: String -> Int
solve2 =
      (\(PosDepthAim p d _) -> p * d)
    . foldl' move (PosDepthAim 0 0 0)
    . parseOrDie commands
  where
    move :: PosDepthAim -> Command -> PosDepthAim
    move pda@(PosDepthAim p d a) (Command op v) = case op of
        Down    -> pda {aim = a + v}
        Up      -> pda {aim = a - v}
        Forward -> pda {pos = p + v, depth = d + (a * v)}
