{-# LANGUAGE LambdaCase #-}

module Y15.D13 where

import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as M
import           Data.List                     (nub, permutations, sort)
import           Data.List.Split               (divvy)
import           Data.Maybe                    (fromMaybe)
import           Text.ParserCombinators.Parsec (Parser, digit, endBy, letter,
                                                many, string)
import           Util

type Guest = String
type Attr = ((Guest, Guest), Int) -- (from, to), attractiveness)

-- Alice would lose 75 happiness units by sitting next to David.
-- Alice would gain 71 happiness units by sitting next to Eric.
attrs :: Parser [Attr]
attrs =
    attr `endBy` eol
  where
    attr :: Parser Attr
    attr =
        (\from sign n to -> ((from, to), sign * n))
        <$> many letter              <* string " would "
        <*> (s2sign <$> many letter) <* string " " -- gain / lose
        <*> (read <$> many digit)    <* string " happiness units by sitting next to "
        <*> many letter              <* string "."

    s2sign = \case
        "gain" -> 1
        "lose" -> -1
        x      -> error $ "Unknown sign: " ++ x

attrs2guests :: [Attr] -> [Guest]
attrs2guests = sort . nub . map (fst . fst)

maxHappiness :: [Attr] -> Int
maxHappiness ms =
    maximum $ table2happiness <$> permutations (attrs2guests ms)
  where
    fta :: M.HashMap (Guest, Guest) Int -- (from, to) -> attractiveness
    fta = M.fromList ms

    table2happiness :: [Guest] -> Int
    table2happiness guests = sum
        $ map (\[a,b] -> fta ! (a,b) + fta ! (b,a))
        $ divvy 2 1
        $ guests ++ take 1 guests -- wrap around one guest

    -- like Data.HashMap.Strict.!, but prints missing key in case of error
    (!) :: (Eq k, Hashable k, Show k) => M.HashMap k v -> k -> v
    (!) m k = fromMaybe
        (error $ "Couldn't find key: " ++ show k)
        (M.lookup k m)

solve1 :: String -> Int
solve1 = maxHappiness . parseOrDie attrs

solve2 :: String -> Int
solve2 = maxHappiness . addSelf . parseOrDie attrs
  where
    addSelf ms =
        ms ++ map (\g -> (("Me", g), 0)) (attrs2guests ms)
           ++ map (\g -> ((g, "Me"), 0)) (attrs2guests ms)

