module Y15.D07 where

import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Bits           (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.HashMap.Strict as M

import           Imports
import           Parser

type RefId = String
type Def   = (RefId, Exp)

data Exp
    = Val Word16
    | Ref RefId
    | Not Exp
    | And Exp Exp
    | Or  Exp Exp
    | Lsh Exp Exp
    | Rsh Exp Exp
    deriving Show

-- 44430 -> b
-- NOT dq -> dr
-- eg AND ei -> ej
-- ep OR eo -> eq
-- lf RSHIFT 2 -> lg
-- kf LSHIFT 15 -> kj
defs :: Parser [Def]
defs =
    def `endBy` eol
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

eval :: Exp -> State (HashMap RefId Exp) Word16
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

apply1 :: HashMap RefId Exp -> Word16
apply1 = evalState (eval $ Ref "a")

apply2 :: HashMap RefId Exp -> Word16
apply2 m = evalState (eval $ Ref "a") $ M.insert "b" (Val $ apply1 m) m

solve1 :: String -> Int
solve1 = fromIntegral . apply1 . M.fromList <$> parseOrDie defs

solve2 :: String -> Int
solve2 = fromIntegral . apply2 . M.fromList <$> parseOrDie defs
