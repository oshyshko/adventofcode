module Y21.D02 where

import           Imports
import           Parser

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
    command = Command <$> op <* pad <*> natural

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
    move pd@(PosDepth{pos,depth}) (Command{op,value}) = case op of
        Down    -> pd {depth = depth + value}
        Up      -> pd {depth = depth - value}
        Forward -> pd {pos   = pos + value}

solve2 :: String -> Int
solve2 =
      (\(PosDepthAim p d _) -> p * d)
    . foldl' move (PosDepthAim 0 0 0)
    . parseOrDie commands
  where
    move :: PosDepthAim -> Command -> PosDepthAim
    move pda@(PosDepthAim{pos,depth,aim}) (Command{op,value}) = case op of
        Down    -> pda {aim = aim + value}
        Up      -> pda {aim = aim - value}
        Forward -> pda {pos = pos + value, depth = depth + (aim * value)}
