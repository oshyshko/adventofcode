module Y15.D19 where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Imports
import           Util

type Part     = String
type Molecule = String

replacementsAndInitial :: Parser ([(Part, Part)], Molecule)
replacementsAndInitial = do
    replacements <- manyTill (mapping <* eol) eol
    initial <- many1 letter
    _ <- eol
    return (replacements, initial)
  where
    mapping :: Parser (Part, Part)
    mapping = (,)
        <$> many1 letter <* many1 space <* string "=>" <* many1 space
        <*> many1 letter

singleReplacements :: Map Part [Part] -> Molecule -> [Molecule]
singleReplacements k2vs =
    go ""
  where
    go _ [] = []
    go stored remaining@(x:xs) =
        (stored <>) <$> replacedHere ++ replacedNext
      where
        replacedNext = go [x] xs
        replacedHere = do
            (pattern, replacements) <- M.toList k2vs
            guard $ pattern `isPrefixOf` remaining
            replacements <&> (<> drop (length pattern) remaining)

solve1 :: String -> Int
solve1 s =
    let (rrs, i) = parseOrDie replacementsAndInitial s
        k2vs = M.fromListWith (++) $ (\(k, v) -> (k, [v])) <$> rrs
        unique = S.toList . S.fromList                              -- faster than nub
    in length . unique . singleReplacements k2vs $ i

solve2 :: String -> Int
solve2 s =
    let (rrs, i) = parseOrDie replacementsAndInitial s
        v2ks = M.fromListWith (++) $ (\(k, v) -> (v, [k])) <$> rrs
    in flip fix [(1, i)] $ \loop -> \case
        [] -> error "no solution"
        ((stepCount,m):remaining) ->
            let replacements = S.fromList $ singleReplacements v2ks m
                smallest     = (stepCount+1,) <$> sortOn length (S.toList replacements)
            in if "e" `S.member` replacements
                then stepCount
                else loop $ smallest ++ remaining
