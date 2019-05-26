module Y15.D09 where

import qualified Data.List                     as L
import qualified Data.Map                      as M

import           Text.ParserCombinators.Parsec (Parser, digit, endBy, letter,
                                                many, parse, string, try, (<|>))

type FromToDist = (String, String, Int)

-- Faerun to Tristram = 65
-- Faerun to Tambi = 129
defs :: Parser [FromToDist]
defs = def `endBy` eol
  where
    def :: Parser FromToDist
    def = (,,) <$> many letter <* string " to "
               <*> many letter <* string " = "
               <*> (read <$> many digit)

    eol :: Parser String
    eol =   try (string "\n\r")
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

solve1 :: String -> Int
solve1 s = either
    (error . show)
    (minimum . solve')
    (parse defs "defs" s)

solve2 :: String -> Int
solve2 s = either
    (error . show)
    (maximum . solve')
    (parse defs "defs" s)
