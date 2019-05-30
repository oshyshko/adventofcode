{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Y15.D12 where

import           Data.Aeson                 (Value (..), decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Strict        (elems)
import           Data.Maybe                 (fromJust, fromMaybe, maybeToList)
import           Data.Scientific            (toBoundedInteger)

parseJsonOrDie :: String -> Value
parseJsonOrDie s = fromMaybe
    (error $ "Could not parse JSON: " ++ s)
    (decode (BL.pack s))

-- TODO refactor
solve1 :: String -> Int
solve1 =
    sum . nums . fromJust . decode . BL.pack
  where
    nums :: Value -> [Int]
    nums = \case
        Object m -> concatMap nums m
        Array v  -> concatMap nums v
        Number n -> maybeToList . toBoundedInteger $ n
        _        -> []

solve2 :: String -> Int
solve2 =
    sum . nums . fromJust . decode . BL.pack
  where
    nums :: Value -> [Int]
    nums = \case
        Object m -> if "red" `elem` elems m then [] else concatMap nums (elems m)
        Array v  -> concatMap nums v
        Number n -> maybeToList . toBoundedInteger $ n
        _        -> []
