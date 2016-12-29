module Y2015.D07 where

import           Control.Monad.State           (State, evalState, gets, modify)
import           Data.Bits                     (complement, shiftL, shiftR,
                                                (.&.), (.|.))
import qualified Data.Map.Strict               as M
import           Data.Word                     (Word16)

import           Text.ParserCombinators.Parsec (ParseError, Parser, digit,
                                                endBy, letter, many1, parse,
                                                string, try, (<|>))

type RefId = String
type Def = (RefId, Exp)

data Exp = Val Word16
         | Ref RefId
         | Not Exp
         | And Exp Exp
         | Or  Exp Exp
         | Lsh Exp Exp
         | Rsh Exp Exp deriving Show

defs :: Parser [Def]
defs = def `endBy` eol
  where
    def :: Parser Def
    def = do
      left <- ex
      string " -> "
      right <- rv
      case right of Ref refId -> return (refId, left)
                    _         -> error $ "Expected Ref, but got: " ++ show right

    rv :: Parser Exp
    rv = Val . read <$> many1 digit
            <|> Ref <$> many1 letter

    ex :: Parser Exp
    ex =  try (Not <$>      (string "NOT "      *> rv))
      <|> try (Or  <$> rv <* string " OR "     <*> rv)
      <|> try (And <$> rv <* string " AND "    <*> rv)
      <|> try (Lsh <$> rv <* string " LSHIFT " <*> rv)
      <|> try (Rsh <$> rv <* string " RSHIFT " <*> rv)
      <|>      rv

    eol :: Parser String
    eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|>      string "\n"
      <|>      string "\r"

eval :: Exp -> State (M.Map RefId Exp) Word16
eval e = case e of Val v   -> return v
                   Ref r   -> do v <- eval =<< gets (M.! r)
                                 modify $ M.insert r (Val v)
                                 return v
                   Not x   -> complement <$> eval x
                   And x y -> (.&.)  <$> eval x <*> eval y
                   Or  x y -> (.|.)  <$> eval x <*> eval y
                   Lsh x n -> shiftL <$> eval x <*> (fromIntegral <$> eval n)
                   Rsh x n -> shiftR <$> eval x <*> (fromIntegral <$> eval n)

solve1 :: M.Map RefId Exp -> Word16
solve1 = evalState (eval $ Ref "a")

solve2 :: M.Map RefId Exp -> Word16
solve2 m = evalState (eval $ Ref "a") $ M.insert "b" (Val $ solve1 m) m

solve :: String -> [Word16]
solve s =
  case parse defs "defs" s :: Either ParseError [Def] of
    Left e   -> error $ show e
    Right xs -> sequence [ solve1
                         , solve2
                         ] $ M.fromList xs
