module Y15.D15 where

import           Data.Foldable                 (maximumBy)
import           Data.Ord                      (comparing)
import           Text.ParserCombinators.Parsec (Parser, char, digit, endBy,
                                                letter, many, parse, space,
                                                string, try, (<|>))
import           Util

data Ingredient = Ingredient
    { name       :: String
    , capacity   :: Int
    , durability :: Int
    , flavor     :: Int
    , texture    :: Int
    , calories   :: Int } deriving (Eq, Show)

type Recipe = [(Ingredient, Int)]

-- Butterscotch: capacity -1, durability -2, flavor  6, texture  3, calories 8
-- Cinnamon:     capacity  2, durability  3, flavor -2, texture -1, calories 3
ingredients :: Parser [Ingredient]
ingredients =
    ingredient `endBy` eol
  where
    ndigit :: Parser Char
    ndigit = char '-' <|> digit
    ingredient :: Parser Ingredient
    ingredient = Ingredient
        <$> many letter
        <* char ':' <* many space <* string "capacity"   <* many space <*> (read <$> many ndigit)
        <* char ',' <* many space <* string "durability" <* many space <*> (read <$> many ndigit)
        <* char ',' <* many space <* string "flavor"     <* many space <*> (read <$> many ndigit)
        <* char ',' <* many space <* string "texture"    <* many space <*> (read <$> many ndigit)
        <* char ',' <* many space <* string "calories"   <* many space <*> (read <$> many ndigit)

-- TODO write tests
-- TODO optimize: iterate once
score :: [Ingredient] -> [Int] -> Int
score ingrs quantities =
    if length ingrs /= length quantities
        then error $ "ingrs and quantites counts don't match: "
            ++ (show . length $ ingrs) ++ " vs "
            ++ (show . length $ quantities)
        else
          (max 0 $ sum (zipWith (\i x -> x * capacity    i) ingrs quantities))
        * (max 0 $ sum (zipWith (\i x -> x * durability  i) ingrs quantities))
        * (max 0 $ sum (zipWith (\i x -> x * flavor     i ) ingrs quantities))
        * (max 0 $ sum (zipWith (\i x -> x * texture    i ) ingrs quantities))

caloriesScore :: [Ingredient] -> [Int] -> Int
caloriesScore ingrs quantities =
    if length ingrs /= length quantities
        then error $ "ingrs and quantites counts don't match: "
            ++ (show . length $ ingrs) ++ " vs "
            ++ (show . length $ quantities)
        else
            sum (zipWith (\i x -> x * calories    i) ingrs quantities)

genRecipes :: Int -> Int -> [[Int]]
genRecipes 0 _ = []
-- recipes _ 0 = []
genRecipes 1 total = [[total]]
genRecipes ingrs total =
    [x : ys | x  <- [0..total],
              ys <- genRecipes (ingrs - 1) (total - x)]
    --
    -- same as:
    -- [0..total] >>= (\x -> [x : ys | ys <- genRecipes (ingrs - 1) (total - x)])
    --
    -- same as:
    -- concatMap
    --     (\x -> [x : ys | ys <- genRecipes (ingrs - 1) (total - x)])
    --     [0..total]

solve1 :: String -> Int
solve1 s =
    let ingrs   = parseOrDie ingredients s
        recipes = genRecipes (length ingrs) 100
    in maximum $ fmap (score ingrs) recipes

solve2 :: String -> Int
solve2 s =
    let ingrs   = parseOrDie ingredients s
        recipes = filter
                    (\r -> caloriesScore ingrs r == 500)
                    (genRecipes (length ingrs) 100)
    in maximum $ fmap (score ingrs) recipes

