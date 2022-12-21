module Y22.D13 where

import           Imports
import           Parser

data Value
    = V Int
    | L [Value]
    deriving (Show, Eq)

-- [[1],[2,3,4]]
value :: Parser Value
value = char '[' *> (L <$> value `sepBy` char ',') <* char ']'
    <|> (V <$> natural)

instance Ord Value where
    compare    (V l)  (V r) = compare l r
    compare    (L l)  (L r) = compare l r
    compare l@(L _) r@(V _) = compare l       (L [r])
    compare l@(V _) r@(L _) = compare (L [l]) r

solve1 :: String -> Int
solve1 =
      sum
    . fmap fst
    . filter ((== LT) . snd)
    . zip [1..]
    . fmap (uncurry compare)
    . parseOrDie (pair `sepBy` eol)
  where
    pair = (,) <$> value <* eol <*> value <* eol

solve2 :: String -> Int
solve2 =
      product
    . catMaybes
    . (\vs -> fmap (fmap succ . (`elemIndex` vs)) ds)
    . sort
    . (++ ds)
    . parseOrDie (value `endBy` many eol)
  where
    ds = parseOrDie value <$> ["[[2]]", "[[6]]"]
