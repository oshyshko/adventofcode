module Day06 where

import           Data.List                     (foldl', find)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe, fromJust)
import           Text.ParserCombinators.Parsec (ParseError, char, digit, endBy,
                                                many, parse, space, string, try,
                                                (<|>))
type Matrix  = M.Map XY Int
type Command = (Op, XY, XY)
data Op      = On | Off | Toggle deriving Show
type XY      = (Int, Int)

-- Examples:
-- parseCommands "turn on 226,196 through 599,390\n"  => Right [(On,    (226,196),(599,390))]
-- parseCommands "turn off 199,133 through 461,193\n" => Right [(Off,   (199,133),(461,193))]
-- parseCommands "toggle 537,781 through 687,941\n"   => Right [(Toggle,(537,781),(687,941))]
parseCommands :: String -> Either ParseError [Command]
parseCommands = parse commands ""
  where
    commands = command `endBy` eol
    command = do
      o <- op
      space
      p0 <- xy
      string " through "
      p1 <- xy
      return (o, p0, p1)

    op =  try (string "turn on")  *> return On
      <|> try (string "turn off") *> return Off
      <|>      string "toggle"    *> return Toggle

    xy = do
      x <- read <$> many digit
      char ','
      y <- read <$> many digit
      return (x,y)

    eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|>      string "\n"
      <|>      string "\r"

applyCommand :: (Op -> Int -> Int) -> Matrix -> Command -> Matrix
applyCommand f mm (op, (x0,y0), (x1,y1)) =
    foldl' (\ m xy -> M.insert xy (f op (fromMaybe 0 $ M.lookup xy m)) m)
           mm
           [ (x,y) | x <- [x0..x1],
                     y <- [y0..y1]]

sumApplyCommands :: (Op -> Int -> Int) -> [Command] -> Int
sumApplyCommands f = sum
                   . map snd
                   . M.toList
                   . foldl' (applyCommand f) M.empty

solve1 :: [Command] -> Int
solve1 = sumApplyCommands (\ op v -> case op of On     -> 1
                                                Off    -> 0
                                                Toggle -> if v == 1 then 0 else 1)

solve2 :: [Command] -> Int
solve2 = sumApplyCommands (\ op v -> case op of On     -> v + 1
                                                Off    -> if v > 0 then v - 1 else 0
                                                Toggle -> v + 2)

main :: IO ()
main = do
  s <- readFile "Day06.txt"
  case parseCommands s of
    Left e   -> error $ show e
    Right xs -> print . sequence [solve2] $ xs
