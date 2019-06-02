module Util where

import           Text.ParserCombinators.Parsec (Parser, parse, string, try,
                                                (<|>))

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|>      string "\n"
    <|>      string "\r"

-- a :: String
-- a = parseOrDie eol "\n\n"
parseOrDie :: Parser a -> String -> a
parseOrDie p s = either
    (error . show)
    id
    (parse p "parseOrDie" s)
