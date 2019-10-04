module Y15.D08 where

import           Data.Functor                  (($>))
import           Text.ParserCombinators.Parsec (Parser, char, endBy, hexDigit,
                                                many, noneOf, (<|>))
import           Util

-- "sjdivfriyaaqa\xd2v\"k\"mpcu\"yyu\"en"
-- "vcqc"
unescapeLines :: Parser [String]
unescapeLines =
    unescapeStr `endBy` eol
  where
    unescapeStr :: Parser String
    unescapeStr = char '"' *> many unescapeChar <* char '"'

    unescapeChar :: Parser Char
    unescapeChar = char '\\' *> (    char  '\\' $> '\\'
                                 <|> char  '\"' $> '"'
                                 <|> (char 'x'  >> hexDigit >> hexDigit >> return '#'))
               <|> noneOf "\""

escapeLines :: Parser [String]
escapeLines =
    escapeStr `endBy` eol
  where
    escapeStr :: Parser String
    escapeStr =  do
        ss <- many $     char    '\\'   $> "\\\\"
                     <|> char    '"'    $> "\\\""
                     <|> ((:[]) <$> noneOf "\n\r")
        return $ "\"" ++ concat ss ++ "\""

solve1 :: String -> Int
solve1 s =
    let xs = parseOrDie unescapeLines s
    in  sum (map length $ lines s) - sum (map length xs)

solve2 :: String -> Int
solve2 s =
    let xs = parseOrDie escapeLines s
    in  sum (map length xs) - sum (map length $ lines s)
