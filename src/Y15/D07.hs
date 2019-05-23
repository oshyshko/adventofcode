{-# LANGUAGE LambdaCase #-}

module Y15.D07 where

import           Control.Monad.State           (State, evalState, gets, modify)
import           Data.Bits                     (complement, shiftL, shiftR,
                                                (.&.), (.|.))
import qualified Data.Map.Strict               as M
import           Data.Word                     (Word16)

import           Text.ParserCombinators.Parsec (Parser, digit, endBy, letter,
                                                many1, parse, string, try,
                                                (<|>))

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
    ex =    try (Not <$>      (string "NOT "      *> rv))
        <|> try (Or  <$> rv <* string " OR "     <*> rv)
        <|> try (And <$> rv <* string " AND "    <*> rv)
        <|> try (Lsh <$> rv <* string " LSHIFT " <*> rv)
        <|> try (Rsh <$> rv <* string " RSHIFT " <*> rv)
        <|> rv

    eol :: Parser String
    eol =   try (string "\n\r")
        <|> try (string "\r\n")
        <|>      string "\n"
        <|>      string "\r"

eval :: Exp -> State (M.Map RefId Exp) Word16
eval = \case
    Val v   -> return v
    Ref r   -> do v <- eval =<< gets (M.! r)
                  modify $ M.insert r (Val v)
                  return v
    Not x   -> complement <$> eval x
    And x y -> (.&.)  <$> eval x <*> eval y
    Or  x y -> (.|.)  <$> eval x <*> eval y
    Lsh x n -> shiftL <$> eval x <*> (fromIntegral <$> eval n)
    Rsh x n -> shiftR <$> eval x <*> (fromIntegral <$> eval n)

apply1 :: M.Map RefId Exp -> Word16
apply1 = evalState (eval $ Ref "a")

apply2 :: M.Map RefId Exp -> Word16
apply2 m = evalState (eval $ Ref "a") $ M.insert "b" (Val $ apply1 m) m

solve1 :: String -> Int
solve1 s = either
    (error . show)
    (fromIntegral . apply1)
    (M.fromList <$> parse defs "defs" s)

solve2 :: String -> Int
solve2 s = either
    (error . show)
    (fromIntegral . apply2)
    (M.fromList <$> parse defs "defs" s)
