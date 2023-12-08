module Y23.D07 where

import           Imports
import           Parser

cardsAndBids :: Parser [(String, Int)]
cardsAndBids =
    cardAndBid `endBy` eol
  where
    cardAndBid = (,) <$> count 5 (oneOf "23456789TJQKA") <* char ' ' <*> natural


groupStrength :: [Int] -> Int
groupStrength g =
      [[5], [4,1], [3,2], [3], [2,2], [2], [1,1,1,1,1]]
    & findIndex (`isPrefixOf` g)
    & fromJust
    & negate

solve :: (Char -> Int) -> (String -> Int) -> String -> Int
solve cardStrength handStrength =
      sum
    . fmap (\(i,(_,bid)) -> i * bid)
    . zip [(1::Int)..]
    . sortOn fst
    . fmap (\(c,b) -> ((handStrength c, fmap cardStrength c), b))
    . parseOrDie cardsAndBids

mkCardStrength :: String -> Char -> Int
mkCardStrength cardOrder c = fromJust . lookup c $ zip cardOrder [(2 :: Int)..]

mkGroup :: String -> [Int]
mkGroup = sortOn negate . fmap length . group . sort

solve1 :: String -> Int
solve1 = solve (mkCardStrength "23456789TJQKA") (groupStrength . mkGroup)

solve2 :: String -> Int
solve2 =
    solve
        (mkCardStrength "J23456789TQKA")
        (groupStrength . \hand ->
            let (jokers,handNoJs) = partition (== 'J') hand
            in case mkGroup handNoJs of
                []     -> [5]
                (x:xs) -> (x + length jokers):xs)
