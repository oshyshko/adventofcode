module Parser
    ( module Control.Applicative
    , module Data.Functor
    , module Text.Parsec
    , module Text.Parsec.String
    , eol
    , getSourceRowCol
    , integer
    , natural
    , pad
    , padded
    , parseOrDie
    ) where

import           Control.Applicative ((<|>))
import           Data.Functor        (($>), (<&>))
import qualified Text.Parsec         as P
import           Text.Parsec         (alphaNum, between, char, count, digit,
                                      endBy, endBy1, getParserState, hexDigit,
                                      letter, lower, many, many1, manyTill,
                                      noneOf, oneOf, option, sepBy, sepEndBy,
                                      sourceColumn, sourceLine, statePos,
                                      string, try, upper, (<?>))
import           Text.Parsec.String  (Parser)

getSourceRowCol :: Parser (P.Column, P.Line)
getSourceRowCol = (\p -> (sourceLine p, sourceColumn p)) . statePos <$> getParserState

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|>      string "\n"
    <|>      string "\r"

natural :: Read a => Parser a
natural = read <$> many1 digit

integer :: Read a => Parser a
integer = read <$> ((<>) <$> option "" (string "-") <*> many1 digit)

pad :: Parser String
pad = many $ char ' '

padded :: Parser a -> Parser a
padded = between pad pad

-- a :: String
-- a = parseOrDie eol "\n\n"
parseOrDie :: Parser a -> String -> a
parseOrDie p s = either
    (error . show)
    id
    (P.parse p "parseOrDie" s)
