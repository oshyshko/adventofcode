module Y22.D14 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser
import           Util            (divvy2, fix2)
import           XY

type Cave = Map XY Bool
data Outcome = Terminate | ContinueOccupied | ContinueFall

-- 498,4 -> 498,6 -> 496,6
-- 503,4 -> 502,4 -> 502,9 -> 494,9
traces :: Parser [[XY]]
traces =
    trace `endBy` eol
  where
    trace = xy `sepBy` padded (string "->")
    xy = XY <$> natural <* char ',' <*> natural

-- Left = infinitely dropping, Right = nowhere to place
fillCave :: (Cave -> XY -> Outcome) -> Cave -> XY -> Either Cave Cave
fillCave at cave startXy =
    fix2 startXy (Right cave) \loop xy ecc ->
        ecc >>= \m -> case at m xy of               -- short-circuit on Left, continue on Right
            Terminate -> Left m
            ContinueOccupied -> Right m
            ContinueFall -> Right m
                & loop (xy + XY   0  1)             -- down
                & loop (xy + XY (-1) 1)             -- down-left
                & loop (xy + XY   1  1)             -- down-right
                & fmap (M.insert xy True)           -- map remaining sand blocks (at rest)

solve :: (Int -> Cave -> XY -> Outcome) -> String -> Int
solve atMaxY s =
    let cave       = parseOrDie traces s
                        & concatMap (divvy2 1)
                        & concatMap (uncurry line2dots)
                        & foldl' (\m xy -> M.insert xy True m) M.empty
        startXy    = XY 500 0
        maxY       = maximum . fmap getY $ startXy : M.keys cave
        filled     = fillCave (atMaxY maxY) cave startXy
        filledSize = M.size case filled of Left n -> n; Right n -> n
    in filledSize - M.size cave
  where
    line2dots ::  XY -> XY -> [XY]
    line2dots a b =
        let dx = signum $ b - a                     -- TODO assert |dx| = 1
            n  = abs (b - a) & \(XY x y) -> x + y
        in take (1+n) (iterate (+ dx) a)

solve1 :: String -> Int
solve1 = solve \maxY m xy@(XY _ y) ->
    if | y > maxY        -> Terminate
       | xy `M.member` m -> ContinueOccupied
       | otherwise       -> ContinueFall

solve2 :: String -> Int
solve2 = solve \maxY m xy@(XY _ y) ->
    if | y >= maxY + 2 || xy `M.member` m -> ContinueOccupied
       | otherwise                        -> ContinueFall