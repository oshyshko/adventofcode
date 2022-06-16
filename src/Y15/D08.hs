module Y15.D08 where

import           Imports
import           Parser

-- "sjdivfriyaaqa\xd2v\"k\"mpcu\"yyu\"en"
-- "vcqc"
unescapeLines :: Parser [String]
unescapeLines =
    unescapeStr `endBy` eol
  where
    unescapeStr :: Parser String
    unescapeStr = char '"' *> many unescapeChar <* char '"'

    unescapeChar :: Parser Char
    unescapeChar =
            char '\\' *> unescapeBackslash
        <|> noneOf "\""

    unescapeBackslash :: Parser Char
    unescapeBackslash =
            char  '\\' $> '\\'
        <|> char  '\"' $> '"'
        <|> (char 'x'  >> hexDigit >> hexDigit >> return '#')

escapeLines :: Parser [String]
escapeLines =
    escapeStr `endBy` eol
  where
    escapeStr :: Parser String
    escapeStr = do
        ss <- many escapeBackslash
        return $ "\"" ++ concat ss ++ "\""

    escapeBackslash :: Parser String
    escapeBackslash =
            char    '\\'   $> "\\\\"
        <|> char    '"'    $> "\\\""
        <|> ((:[]) <$> noneOf "\n\r")

solve1 :: String -> Int
solve1 s =
    let xs = parseOrDie unescapeLines s
    in sum (map length $ lines s) - sum (map length xs)

solve2 :: String -> Int
solve2 s =
    let xs = parseOrDie escapeLines s
    in sum (map length xs) - sum (map length $ lines s)
