module Y22.D14 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser
import           Util            (divvy2, fix2)
import           XY

type Cave = Map XY Bool
data Decision = Answer | Skip | Continue

-- 498,4 -> 498,6 -> 496,6
-- 503,4 -> 502,4 -> 502,9 -> 494,9
traces :: Parser [[XY]]
traces =
    trace' `endBy` eol
  where
    trace' = xy `sepBy` padded (string "->")
    xy = XY <$> natural <* char ',' <*> natural

-- Left = infinitely dropping, Right = nowhere to place
fillCave :: (Cave -> XY -> Decision) -> Cave -> XY -> Either Cave Cave
fillCave decide cave startXy =
    fix2 startXy (Right cave) \loop xy ecc ->
        ecc >>= \m -> case decide m xy of           -- shortcut on Left, continue on Right
            Answer   -> Left m
            Skip     -> Right m
            Continue -> Right m
                & loop (xy + XY   0  1)             -- down
                & loop (xy + XY (-1) 1)             -- down-left
                & loop (xy + XY   1  1)             -- down-right
                & fmap (M.insert xy True)           -- map remaining sand blocks (at rest)

solve :: (Int -> Cave -> XY -> Decision) -> String -> Int
solve decide s =
    let cave           = parseOrDie traces s
                            & concatMap (divvy2 1)
                            & concatMap (uncurry line2dots)
                            & foldl' (\m xy -> M.insert xy True m) M.empty
        startXy        = XY 500 0
        maxY           = foldl' max minBound . fmap getY $ startXy : M.keys cave
        caveFilled     = fillCave (decide maxY) cave startXy
        caveFilledSize = M.size case caveFilled of Left n -> n; Right n -> n
    in caveFilledSize - M.size cave
  where
    line2dots ::  XY -> XY -> [XY]
    line2dots a b =
        let dx = signum $ b - a                     -- TODO assert |dx| = 1
            n  = abs (b - a) & \(XY x y) -> x + y
        in take (1+n) (iterate (+ dx) a)

solve1 :: String -> Int
solve1 = solve \maxY m xy@(XY _ y) ->
    if | y > maxY        -> Answer
       | xy `M.member` m -> Skip
       | otherwise       -> Continue

solve2 :: String -> Int
solve2 = solve \maxY m xy@(XY _ y) ->
    if | y >= maxY + 2 || xy `M.member` m -> Skip
       | otherwise                        -> Continue