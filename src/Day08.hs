module Day08 where

import           Text.ParserCombinators.Parsec (GenParser, ParseError,
                                                char, endBy, hexDigit, many,
                                                noneOf, parse, string, try,
                                                (<|>))

unescapeLines :: GenParser Char st [String]
unescapeLines = unescapeStr `endBy` eol

unescapeStr :: GenParser Char st String
unescapeStr = do char '"'; s <- many unescapeChar; char '"'; return s

unescapeChar :: GenParser Char st Char
unescapeChar = try (do string "\\\\"; return '\\')
           <|> try (do string "\\\""; return '"')
           <|> try (do string "\\x"; _ <- hexDigit; _ <- hexDigit; return '#')
           <|>     noneOf "\""

escapeLines :: GenParser Char st [String]
escapeLines = escapeStr `endBy` eol

escapeStr :: GenParser Char st String
escapeStr =  do ss <- many $  try (do char '\\'; return "\\\\")
                          <|> try (do char '"';  return "\\\"")
                          <|> try (do s <- noneOf "\n\r"; return [s]);
                return $ "\"" ++ concat ss ++ "\""

eol :: GenParser Char st String
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
