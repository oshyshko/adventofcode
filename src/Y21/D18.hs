module Y21.D18 where

import           Imports
import           Util

data Tree
    = Lit Int
    | Pair Tree Tree
    deriving (Show, Eq)

-- [[1,2],3]
tree :: Parser Tree
tree =
    between (char '[') (char ']')
        (Pair <$> litOrPair <* char ',' <*> litOrPair)
  where
    litOrPair = (Lit <$> natural) <|> tree

explode :: Tree -> Maybe Tree
explode =
    fmap (\(a, _, _) -> a) . go 0
  where
    go :: Int -> Tree -> Maybe (Tree, Maybe Int, Maybe Int)
    go n = \case
        Lit _        -> Nothing
        e@(Pair a b) -> case n of
            4 -> case e of                                          -- reached depth 4
                Pair (Lit lv) (Lit rv) -> Just (Lit 0, Just lv, Just rv)
                _ -> error $ "Got unexpected Exp at depth " <> show n <> ": " <> show e

            _ ->     (go (n+1) a <&> \(outA, leftA, rightA) ->      -- try change A (left branch)
                        let outB = maybe b (`waveRight` b) rightA
                        in (Pair outA outB, leftA, Nothing))

                <|>  (go (n+1) b <&> \(outB, leftB, rightB) ->      -- otherwise, try change B (right branch)
                        let outA = maybe a (`waveLeft` a) leftB
                        in (Pair outA outB, Nothing, rightB))

    waveLeft d = \case
        Lit x    -> Lit $ x + d
        Pair a b -> Pair a (waveLeft d b)

    waveRight d = \case
        Lit x    -> Lit $ x + d
        Pair a b -> Pair (waveRight d a) b

split :: Tree -> Maybe Tree
split (Lit x)
    | x < 10    = Nothing
    | otherwise = let (q, r) = divMod x 2 in Just $ Pair (Lit q) (Lit $ q + r)

split (Pair a b) =
        (split a <&> (`Pair` b))
    <|> (split b <&> Pair a)

add :: Tree -> Tree -> Tree
add a b =
    reduce $ Pair a b
  where
    reduce t = maybe t reduce (explode t <|> split t)

magnitude :: Tree -> Int
magnitude (Lit x)    = x
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

solve1, solve2 :: String -> Int
solve1 = magnitude . foldl1 add . fmap (parseOrDie tree) . lines
solve2 s =
    let ns = parseOrDie tree <$> lines s
    in maximum
        [ magnitude x
        | a <- ns
        , b <- ns
        , a /= b
        , x <- [add a b, add b a]
        ]
