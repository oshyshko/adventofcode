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
            4 -> case e of                                                          -- reached depth 4
                Pair (Lit lv) (Lit rv) -> Just (Lit 0, Just lv, Just rv)
                _ -> error $ "Got unexpected Exp at depth " <> show n <> ": " <> show e

            _ -> go (n+1) a & \case                                                 -- try change A (left branch)
                Just (outA, leftA, rightA) ->                                       -- A changed
                    let outB = maybe b (`waveRight` b) rightA
                    in Just (Pair outA outB, leftA, Nothing)

                Nothing ->                                                          -- A not changed (for now)
                    go (n+1) b & \case                                              -- try change B (right branch)
                        Just (outB, leftB, rightB) ->
                            let outA = maybe a (`waveLeft` a) leftB
                            in Just (Pair outA outB, Nothing, rightB)
                        Nothing -> Nothing
    waveLeft d = \case
        Lit x    -> Lit $ x + d
        Pair a b -> Pair a (waveLeft d b)

    waveRight d = \case
        Lit x    -> Lit $ x + d
        Pair a b -> Pair (waveRight d a) b

split :: Tree -> Maybe Tree
split = \case
    Lit x -> if x >= 10
        then Just $ Pair (Lit $ x `div` 2) (Lit $ ceiling (fromIntegral x / 2::Float))
        else Nothing
    Pair a b -> split a & \case
        Just na -> Just $ Pair na b
        Nothing -> split b & \case
            Just nb -> Just $ Pair a nb
            Nothing -> Nothing

add :: Tree -> Tree -> Tree
add a b =
    go $ Pair a b
  where
    go e = explode e & \case
        Just ne -> go ne
        Nothing -> split e & \case
            Just ne -> go ne
            Nothing -> e

magnitude :: Tree -> Int
magnitude = \case
    Lit x    -> x
    Pair a b -> 3 * magnitude a + 2 * magnitude b

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
