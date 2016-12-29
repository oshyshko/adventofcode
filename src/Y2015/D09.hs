module Y2015.D09 where

import qualified Data.List                     as L
import qualified Data.Map                      as M

import           Text.ParserCombinators.Parsec (ParseError, Parser, digit,
                                                endBy, letter, many, parse,
                                                string, try, (<|>))

type FromToDist = (String, String, Int)

defs :: Parser [FromToDist]
defs = def `endBy` eol
  where
    def :: Parser FromToDist
    def = (,,) <$> many letter <* string " to "
               <*> many letter <* string " = "
               <*> (read <$> many digit)

    eol :: Parser String
    eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|>      string "\n"
      <|>      string "\r"

solve' :: [FromToDist] -> [Int]
solve' ftds =
  let locations      = L.nub      $ concat [ [a,b]        | (a,b,_) <- ftds ]
      ab2dist        = M.fromList $ concat [ [((a,b), d),
                                              ((b,a), d)] | (a,b,d) <- ftds ]
      links     path = zip path (tail path)
      path2dist path = sum $ map (ab2dist M.!) (links path)
   in map path2dist (L.permutations locations)

solve :: String -> [Int]
solve s =
  case parse defs "defs" s :: Either ParseError [FromToDist] of
    Left e   -> error $ show e
    Right xs -> sequence [ minimum . solve'
                         , maximum . solve'
                         ] xs
