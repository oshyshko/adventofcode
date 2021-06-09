module Y15.D16 where

import           Util
import           Imports

data Key
    = Akitas
    | Cars
    | Cats
    | Children
    | Goldfish
    | Perfumes
    | Pomeranians
    | Samoyeds
    | Trees
    | Vizslas
    deriving (Eq, Ord, Show)

type Clue      = (Key, Int)
type Sue2Clues = (Int, [Clue])

-- Sue 1: goldfish: 6, trees: 9, akitas: 0
-- Sue 2: goldfish: 7, trees: 1, akitas: 0
sue2clues :: Parser [Sue2Clues]
sue2clues =
    sue2clue `endBy` eol
  where
    sue2clue :: Parser Sue2Clues
    sue2clue = do
        string "Sue" *> many space
        sueId <- read <$> many digit
        many space <* char ':' <* many space
        clues <- clue `sepBy` (char ',' <* many space) :: Parser [Clue]
        return (sueId, clues)
    clue :: Parser Clue
    clue = do
        -- TODO refactor: generalize
        k <-    try (string "children"    $> Children)
            <|> try (string "cats"        $> Cats)
            <|> try (string "samoyeds"    $> Samoyeds)
            <|> try (string "pomeranians" $> Pomeranians)
            <|> try (string "akitas"      $> Akitas)
            <|> try (string "vizslas"     $> Vizslas)
            <|> try (string "goldfish"    $> Goldfish)
            <|> try (string "trees"       $> Trees)
            <|> try (string "cars"        $> Cars)
            <|> try (string "perfumes"    $> Perfumes)

        string ":" <* many space

        v <- read <$> many digit
        return (k, v)

exactlyOneOrDie :: Show a => [a] -> a
exactlyOneOrDie = \case
    [x] -> x
    xs  -> error $ "Expected a list with exactly one element, but got with "
        ++ (show . length $ xs) ++ ": " ++ show xs

solve1 :: String -> Int
solve1 s =
      parseOrDie sue2clues s
    & filter (\(_, clues) -> all satisfies clues)
    & exactlyOneOrDie
    & fst
  where
    satisfies :: Clue -> Bool
    satisfies (k, v) =
        v == case k of
            Children    -> 3
            Cats        -> 7
            Samoyeds    -> 2
            Pomeranians -> 3
            Akitas      -> 0
            Vizslas     -> 0
            Goldfish    -> 5
            Trees       -> 3
            Cars        -> 2
            Perfumes    -> 1

solve2 :: String -> Int
solve2 s =
      parseOrDie sue2clues s
    & filter (\(_, clues) -> all satisfies clues)
    & exactlyOneOrDie
    & fst
  where
    satisfies :: Clue -> Bool
    satisfies (k, v) =
        v & case k of
            Children    -> (== 3)
            Cats        -> (>  7)
            Samoyeds    -> (== 2)
            Pomeranians -> (<  3)
            Akitas      -> (== 0)
            Vizslas     -> (== 0)
            Goldfish    -> (<  5)
            Trees       -> (>  3)
            Cars        -> (== 2)
            Perfumes    -> (== 1)
