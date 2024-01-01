module Y23.D03 where

import           Geom.XY
import           Imports
import           Parser

data Part   = Part   { partId  ::Int,  xy::XY, wh::WH } deriving Show
data Symbol = Symbol { symbolId::Char, xy::XY         } deriving Show

-- 467..114..
-- ...*......
partsAndSymbols :: Parser ([Part], [Symbol])
partsAndSymbols =
        many1 (between empty empty (Left <$> part <|> Right <$> symbol))
    <&> partitionEithers
  where
    empty  = many (string "." <|> eol)
    part   = getSourceRowCol >>= \(r,c) -> natural            <&> \i -> Part   i (XY c r) (XY (length $ show i) 1)
    symbol = getSourceRowCol >>= \(r,c) -> oneOf "#$%&*+-/=@" <&> \i -> Symbol i (XY c r)

containsOrTouches :: Part -> Symbol -> Bool
containsOrTouches (Part _ (XY x y) (XY w h)) (Symbol _ (XY q p)) =
    (x-1) <= q && q < (x+1+w) && (y-1) <= p && p < (y+1+h)

solve1 :: String -> Int
solve1 input =
    let (parts,symbols) = parseOrDie partsAndSymbols input
    in    parts
        & filter (\p -> any (containsOrTouches p) symbols)
        & fmap partId
        & sum

solve2 :: String -> Int
solve2 input =
    let (parts,symbols) = parseOrDie partsAndSymbols input
    in    symbols
        & filter ((== '*') . symbolId)
        & fmap (\s ->
            let partsTouchingGear = filter (`containsOrTouches` s) parts
            in if 2 == length partsTouchingGear
                then product $ partId <$> partsTouchingGear
                else 0)
        & sum
