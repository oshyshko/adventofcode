module Y21.D14 where

import qualified Data.Map.Strict as M

import           Imports
import           Parser
import           Util            (divvy2)

type Elem = Char
type Poly = [Elem]
data Pair = Pair Elem Elem deriving (Eq, Ord, Show)
data Rule = Rule Pair Elem deriving (Show)

-- NNCB
--
-- CH -> B
-- HH -> N
--
polyAndRules :: Parser (Poly, [Rule])
polyAndRules =
    (,) <$> poly <* eol <* eol
        <*> rule `endBy` eol
  where
    poly = many1 letter
    rule = (\a b -> Rule (Pair a b))
        <$> letter <*> letter
        <* string " -> "
        <*> letter

tick :: Map Pair [Pair] -> Map Pair Int -> Map Pair Int
tick rules =
    M.fromListWith (+) . concatMap replace . M.toList
  where
    replace :: (Pair, Int) -> [(Pair, Int)]
    replace pv@(p,v) = maybe [pv] (fmap (,v)) (M.lookup p rules)

solve :: Int -> Poly -> Int
solve n =
      score
    . (!! n)
    . (\(poly, rules) -> iterate (tick $ rules2map rules) (poly2map poly))
    . parseOrDie polyAndRules
  where
    rules2map :: [Rule] -> Map Pair [Pair]
    rules2map = M.fromList . fmap (\(Rule ab@(Pair a b) r) -> (ab, [Pair a r, Pair r b]))

    poly2map :: Poly -> Map Pair Int
    poly2map poly = M.fromListWith (+) $ (\(a,b) -> (Pair a b, 1)) <$> divvy2 1 (poly <> "$")

    score :: Map Pair Int -> Int
    score =
            (uncurry (-) . (maximum &&& minimum))
        . M.elems
        . M.fromListWith (+)
        . fmap (\(Pair a _, v) -> (a,v))
        . M.toList

solve1, solve2 :: String -> Int
solve1 = solve 10
solve2 = solve 40
