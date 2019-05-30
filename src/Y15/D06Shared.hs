module Y15.D06Shared where

import           Data.Array.MArray             (MArray, readArray, writeArray)
import           Data.Functor                  (($>))
import           Text.Parsec.Error             (ParseError)
import           Text.ParserCombinators.Parsec (Parser, char, digit, endBy,
                                                many, parse, space, string, try,
                                                (<|>))

side :: Int -- TODO determine sides from input?
side = 1000

type Command = (Op, XY, XY)
data Op      = On | Off | Toggle deriving Show
type XY      = (Int, Int)

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
commands :: Parser [Command]
commands = command `endBy` eol
  where
    command :: Parser Command
    command = (,,) <$> op <* space
                   <*> xy <* string " through "
                   <*> xy

    op :: Parser Op
    op =    try (string "turn on")  $> On
        <|> try (string "turn off") $> Off
        <|>      string "toggle"    $> Toggle

    xy :: Parser XY
    xy = (,) <$> (read <$> many digit)
             <* char ','
             <*> (read <$> many digit)

    eol :: Parser String
    eol =   try (string "\n\r")
        <|> try (string "\r\n")
        <|>      string "\n"
        <|>      string "\r"

{-# INLINE apply1 #-}
apply1 :: Op -> Int -> Int
apply1 op v = case op of
    On     -> 1
    Off    -> 0
    Toggle -> if v == 1 then 0 else 1

{-# INLINE apply2 #-}
apply2 :: Op -> Int -> Int
apply2 op v = case op of
    On     -> v + 1
    Off    -> if v > 0 then v - 1 else 0
    Toggle -> v + 2

parseCommands :: String -> Either ParseError [Command]
parseCommands = parse commands "commands"

{-# INLINE applyCommand #-}
applyCommand :: MArray a t m => (Op -> t -> t) -> a Int t -> Command -> m ()
applyCommand f m (op, (x0,y0), (x1,y1)) =
    mapM_ (\ xy -> do
              v <- get m xy
              set m xy (f op v))

          [ (x,y) | y <- [y0..y1],
                    x <- [x0..x1]]
  where
    get :: MArray a e m => a Int e -> XY -> m e
    get a (x,y) = readArray a (x + y * side)

    set :: MArray a e m => a Int e -> XY -> e -> m ()
    set a (x,y) = writeArray a (x + y * side)

