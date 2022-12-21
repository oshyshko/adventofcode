module Y22.D13 where

import           Imports
import           Parser

data Packet
    = V Int
    | L [Packet]
    deriving (Show, Eq)

-- [[1],[2,3,4]]
packet :: Parser Packet
packet = char '[' *> (L <$> packet `sepBy` char ',') <* char ']'
    <|> (V <$> natural)

instance Ord Packet where
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
    pair = (,) <$> packet <* eol <*> packet <* eol

solve2 :: String -> Int
solve2 =
      product
    . catMaybes
    . (\vs -> fmap (fmap succ . (`elemIndex` vs)) ds)
    . sort
    . (++ ds)
    . parseOrDie (packet `endBy` many eol)
  where
    ds = parseOrDie packet <$> ["[[2]]", "[[6]]"]
