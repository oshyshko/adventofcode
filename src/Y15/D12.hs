module Y15.D12 where

import           Data.Aeson                 (Object, Value (..), decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Strict        (elems)
import           Data.Maybe                 (fromJust, maybeToList)
import           Data.Scientific            (toBoundedInteger)

solve :: (Object -> Bool) -> String -> Int
solve digObject =
    sum . nums . fromJust . decode . BL.pack
  where
    nums :: Value -> [Int]
    nums = \case
        Object m -> if digObject m then concatMap nums (elems m) else []
        Array v  -> concatMap nums v
        Number n -> maybeToList . toBoundedInteger $ n
        _        -> []

solve1 :: String -> Int
solve1 = solve $ const True

solve2 :: String -> Int
solve2 = solve $ \m -> "red" `notElem` elems m
