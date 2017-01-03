module Y2015.D08 where

import           Text.ParserCombinators.Parsec (Parser, char, endBy, hexDigit,
                                                many, noneOf, parse, string,
                                                try, (<|>))

unescapeLines :: Parser [String]
unescapeLines = unescapeStr `endBy` eol

unescapeStr :: Parser String
unescapeStr = do char '"'; s <- many unescapeChar; char '"'; return s

unescapeChar :: Parser Char
unescapeChar = char   '\\' *> (    char  '\\' *> return '\\'
                               <|> char  '\"' *> return '"'
                               <|> (char 'x'  >> hexDigit >> hexDigit >> return '#'))
           <|> noneOf "\""

escapeLines :: Parser [String]
escapeLines = escapeStr `endBy` eol

escapeStr :: Parser String
escapeStr =  do ss <- many $     char    '\\'   *>  return "\\\\"
                             <|> char    '"'    *>  return "\\\""
                             <|> (noneOf "\n\r" >>= return . (:[]))
                return $ "\"" ++ concat ss ++ "\""

eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|>      string "\n"
  <|>      string "\r"

solve1 :: String -> Int
solve1 s  = either (error . show)
                   (\xs -> sum (map length $ lines s) - sum (map length xs))
                   (parse unescapeLines "escapeLines" s)

solve2 :: String -> Int
solve2 s  = either (error . show)
                   (\xs -> sum (map length xs) - sum (map length $ lines s))
                   (parse escapeLines "escapeLines" s)
