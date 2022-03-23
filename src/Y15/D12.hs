module Y15.D12 where

import           Data.Aeson                 (Object, Value (..), decode)
import qualified           Data.Aeson.KeyMap                 as M
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Scientific            (toBoundedInteger)

import           Imports

solve :: (Object -> Bool) -> String -> Int
solve digObject =
    sum . nums . fromJust . decode . BL.pack
  where
    nums :: Value -> [Int]
    nums = \case
        Object m -> if digObject m then concatMap nums (M.elems m) else []
        Array v  -> concatMap nums v
        Number n -> maybeToList . toBoundedInteger $ n
        _        -> []

solve1 :: String -> Int
solve1 = solve $ const True

solve2 :: String -> Int
solve2 = solve $ \m ->  M.member "red" m
