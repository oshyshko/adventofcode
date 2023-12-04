module Y23.D04 where

import qualified Data.Set as S
import           Parser

data Card = Card
    { winners :: [Int]
    , have    :: [Int]
    } deriving Show

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
-- Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
cards :: Parser [Card]
cards =
    card `endBy` eol
  where
    card :: Parser Card
    card = Card
        <$ string "Card" <* padded (natural @Int) <* string ":"
        <*> many1 (padded natural) <* string "|"
        <*> many1 (padded natural)

matchCount :: Card -> Int
matchCount (Card{winners,have}) =
    length $ S.fromList winners `S.intersection` S.fromList have

solve1 :: String -> Int
solve1 =
    sum . fmap worth . parseOrDie cards
  where
    worth c =
        let m = matchCount c
        in if m == 0 then 0 else 2 ^ (m-1)

solve2 :: String -> Int
solve2 =
    sum . f . fmap (\x -> (matchCount x, 1::Int)) . parseOrDie cards
  where
    f [] = []
    f ((m,c):xs) =                                  -- (m)atch-count, (c)ard-count
        let (nextM,rest) = splitAt m xs             -- only add to `nextM` cards, keep `rest` unchanged
        in c : f (fmap (addCount c) nextM ++ rest)  -- pop-out (c)ard-count
    addCount c = fmap (+c)
