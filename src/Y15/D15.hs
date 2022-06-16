module Y15.D15 where

import           Parser

data Ingredient = Ingredient
    { name       :: String
    , capacity   :: Int
    , durability :: Int
    , flavor     :: Int
    , texture    :: Int
    , calories   :: Int
    } deriving (Eq, Show)

-- Butterscotch: capacity -1, durability -2, flavor  6, texture  3, calories 8
-- Cinnamon:     capacity  2, durability  3, flavor -2, texture -1, calories 3
ingredients :: Parser [Ingredient]
ingredients =
    ingredient `endBy` eol
  where
    ingredient :: Parser Ingredient
    ingredient = Ingredient
        <$> many letter
        <* char ':' <* padded (string "capacity")   <*> integer
        <* char ',' <* padded (string "durability") <*> integer
        <* char ',' <* padded (string "flavor")     <*> integer
        <* char ',' <* padded (string "texture")    <*> integer
        <* char ',' <* padded (string "calories")   <*> integer

-- TODO optimize: iterate once
score :: [Ingredient] -> [Int] -> Int
score ingrs mix =
    assertSameLengths "ingrs and mix" ingrs mix $
          max 0 (sum (zipWith (\i x -> x * capacity   i) ingrs mix))
        * max 0 (sum (zipWith (\i x -> x * durability i) ingrs mix))
        * max 0 (sum (zipWith (\i x -> x * flavor     i) ingrs mix))
        * max 0 (sum (zipWith (\i x -> x * texture    i) ingrs mix))

calsIn :: [Ingredient] -> [Int] -> Int
calsIn ingrs mix =
    assertSameLengths "ingrs and mix" ingrs mix $
        sum (zipWith (\i x -> x * calories i) ingrs mix)

assertSameLengths :: (Foldable q, Foldable p) => String -> q a -> p b -> c -> c
assertSameLengths xsAndYsPrefix xs ys a =
    if length xs /= length ys
        then error $ xsAndYsPrefix ++ " counts expected to match, but they didn't: "
            ++ (show . length $ xs) ++ " vs "
            ++ (show . length $ ys)
        else a

genMixes :: Int -> Int -> [[Int]]
genMixes 0         _          = []
genMixes 1         spoonsLeft = [[spoonsLeft]]
genMixes ingrsLeft spoonsLeft =
    [ x:rest
    | x    <- [0..spoonsLeft]
    , rest <- genMixes (ingrsLeft - 1) (spoonsLeft - x)
    ]

solve1 :: String -> Int
solve1 s =
    let ingrs = parseOrDie ingredients s
        mixes = genMixes (length ingrs) 100
    in maximum $ score ingrs <$> mixes

solve2 :: String -> Int
solve2 s =
    let ingrs = parseOrDie ingredients s
        mixes = filter
                    (\mix -> calsIn ingrs mix == 500)
                    (genMixes (length ingrs) 100)
    in maximum $ score ingrs <$> mixes
