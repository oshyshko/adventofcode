module Y23.D11 where

import qualified Data.Set as S
import           Geom.XY
import           Imports
import           Parser
import           Util

stars :: Parser [XY]
stars =
    many1 (between empty empty star)
  where
    empty = many (string "." <|> eol)
    star  = getSourceRowCol >>= \(r,c) -> string "#" $> XY c r

-- findSkipped [1,3,5] => [2,4]
findSkipped :: (Ord a, Enum a) => [a] -> [a]
findSkipped xs =
    let s = S.fromList xs
    in S.toList $ S.fromList [S.findMin s .. S.findMax s] S.\\ s

solve :: Int -> String -> Int
solve n s =
    let xys             = parseOrDie stars s
        skippedXs       = findSkipped $ fmap getX xys
        skippedYs       = findSkipped $ fmap getY xys
        betweenAB a b x = min a b < x && x < max a b
        expansionsBetween (XY ax ay) (XY bx by) =
            length $ filter (betweenAB ax bx) skippedXs
                  <> filter (betweenAB ay by) skippedYs
    in    tuples2 xys
        & fmap (\(a, b) -> distanceManhattan a b + n * expansionsBetween a b)
        & sum

solve1, solve2 :: String -> Int
solve1 = solve 1
solve2 = solve $ 1000000 - 1
