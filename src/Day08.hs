module Day08 where

import           Text.ParserCombinators.Parsec (Parser, ParseError,
                                                char, endBy, hexDigit, many,
                                                noneOf, parse, string, try,
                                                (<|>))

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
solve1 s  =   case parse unescapeLines "unescapeLines" s :: Either ParseError [String] of
    Left e   -> error $ show e
    Right xs -> sum (map length $ lines s) - sum (map length xs)

solve2 :: String -> Int
solve2 s  =   case parse escapeLines "escapeLines" s :: Either ParseError [String] of
    Left e   -> error $ show e
    Right xs -> sum (map length xs) - sum (map length $ lines s)

main :: IO ()
main = do
  s <- readFile "Day08.txt"
  print [solve1 s, solve2 s]
