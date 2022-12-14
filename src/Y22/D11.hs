module Y22.D11 where

import qualified Data.Map.Strict as M
import           Data.Ord        (Down (..))
import           Imports
import           Parser

type Index = Int
type Worry = Int
type State = (Map Index [Worry], Map Index Int)     -- (items, inspections)

data Monkey = Monkey
    { index       :: Index
    , items       :: [Worry]
    , operation   :: (Arg, Fn, Arg)
    , divisibleBy :: Worry
    , onTrue      :: Index
    , onFalse     :: Index
    } deriving Show

data Arg = Old | Lit Worry deriving Show
data Fn  = Add | Mul deriving Show

-- Monkey 0:
--   Starting items: 79, 98
--   Operation: new = old * 19
--   Test: divisible by 23
--     If true: throw to monkey 2
--     If false: throw to monkey 3
monkeys :: Parser [Monkey]
monkeys =
    monkey `sepBy` eol
  where
    monkey = Monkey
        <$> (       string "Monkey"                     *> padded natural <* char ':'           <* eol)
        <*> (pad *> string "Starting items:"            *> (padded natural `sepBy` char ',')    <* eol)
        <*> (pad *> string "Operation: new ="           *> ((,,) <$> arg <*> fn <*> arg)        <* eol)
        <*> (pad *> string "Test: divisible by"         *> padded natural                       <* eol)
        <*> (pad *> string "If true: throw to monkey"   *> padded natural                       <* eol)
        <*> (pad *> string "If false: throw to monkey"  *> padded natural                       <* eol)
    arg = padded $ (Old <$ string "old") <|> (Lit <$> natural)
    fn  = padded $ (Add <$ string "+")   <|> (Mul <$ string "*")

playAllMonkeys :: (Worry -> Worry) -> Map Index Monkey -> State -> State
playAllMonkeys dropWorry i2m (i2is,i2vs) =
    foldl'
        (\ms index -> playMonkey dropWorry (i2m M.! index) ms)
        (i2is, i2vs)
        (sort $ M.keys i2is)

playMonkey :: (Worry -> Worry) -> Monkey -> State -> State
playMonkey dropWorry m@Monkey{index} (i2is,i2vs) =
    let is = i2is M.! index
        i2isNew = foldl'
            (\i2is' worry ->
                let (insertIndex,newWorry) = whereTo dropWorry m worry
                in M.insertWith (++) insertIndex [newWorry] i2is')
            (M.insert index [] i2is)
            is
    in (i2isNew, M.insertWith (+) index (length is) i2vs)

whereTo :: (Worry -> Worry) -> Monkey -> Worry -> (Index, Worry)
whereTo dropWorry Monkey{operation,divisibleBy,onTrue,onFalse} worry =
    let (a,f,b)      = operation
        worryRaised  = mapFn f (mapArg worry a) (mapArg worry b)
        worryDropped = dropWorry worryRaised
        throwToIndex = if worryDropped `rem` divisibleBy == 0 then onTrue else onFalse
    in (throwToIndex, worryDropped)
  where
    mapFn      = \case Add -> (+); Mul -> (*)
    mapArg old = \case Old -> old; Lit x -> x

solve :: Int -> (Worry -> Worry) -> [Monkey] -> Int
solve n dropWorry ms =
      fmap (\m@Monkey{index} -> (index, m)) ms
    & M.fromList                    -- Map Index Monkey
    & states                        -- [State]
    & (!! n)                        -- (Map Index [W], Map Index Int)
    & snd
    & M.elems                       -- inspections by every monkey
    & sortOn Down
    & take 2
    & product
  where
    states :: Map Index Monkey -> [State]
    states i2m =
        iterate
            (playAllMonkeys dropWorry i2m)
            (items <$> i2m, M.empty)

solve1 :: String -> Int
solve1 = solve 20 (`quot` 3) . parseOrDie monkeys

solve2 :: String -> Int
solve2 s =
    let ms = parseOrDie monkeys s
        wraparound = (`rem` foldl' lcm 1 (fmap divisibleBy ms))
    in solve 10000 wraparound ms
