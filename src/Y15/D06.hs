module Y15.D06 where

import           Data.List                     (foldl')
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe)

import           Text.ParserCombinators.Parsec (Parser, char, digit, endBy,
                                                many, parse, space, string, try,
                                                (<|>))

type Matrix  = M.Map XY Int

type Command = (Op, XY, XY)
data Op      = On | Off | Toggle deriving Show
type XY      = (Int, Int)

commands :: Parser [Command]
commands = command `endBy` eol
  where
    command :: Parser Command
    command = (,,) <$> op <* space
                   <*> xy <* string " through "
                   <*> xy

    op :: Parser Op
    op =  try (string "turn on")  *> return On
      <|> try (string "turn off") *> return Off
      <|>      string "toggle"    *> return Toggle

    xy :: Parser XY
    xy = (,) <$> (read <$> many digit) <* char ','
             <*> (read <$> many digit)

    eol :: Parser String
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

apply1 :: Op -> Int -> Int
apply1 op v = case op of On     -> 1
                         Off    -> 0
                         Toggle -> if v == 1 then 0 else 1

apply2 :: Op -> Int -> Int
apply2 op v = case op of On     -> v + 1
                         Off    -> if v > 0 then v - 1 else 0
                         Toggle -> v + 2

solve1 :: String -> Int
solve1 s = either (error . show)
                  (sumApplyCommands apply1)
                  (parse commands "commands" s)

solve2 :: String -> Int
solve2 s = either (error . show)
                  (sumApplyCommands apply2)
                  (parse commands "commands" s)
