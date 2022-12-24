{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module Y22.D14 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser
import           Util            (divvy2, fix2)
import           XY

type Cave = Map XY Bool

parseCave :: String -> Cave
parseCave =
      foldl' (\m xy -> M.insert xy True m) M.empty
    . concatMap (\(a,b) -> line2dots a b)
    . concatMap (divvy2 1)
    . parseOrDie traces
  where
    -- 498,4 -> 498,6 -> 496,6
    -- 503,4 -> 502,4 -> 502,9 -> 494,9
    traces :: Parser [[XY]]
    traces =
        trace' `endBy` eol
      where
        trace' = xy `sepBy` padded (string "->")
        xy = XY <$> natural <* char ',' <*> natural

    line2dots ::  XY -> XY -> [XY]
    line2dots a b =
        let dx = signum $ b - a                             -- TODO assert |dx| = 1
            n  = abs (b - a) & \(XY x y) -> x + y
        in take (1+n) (iterate (+ dx) a)

fillCave
    :: (XY -> XY -> Cave -> XY -> Bool)
    -> (XY -> XY -> Cave -> XY -> Bool)
    -> Cave
    -> XY
    -> Either Cave Cave
fillCave stop occupied cave start =
    let minXy = foldl' (xyBiMap min) maxBound $ start : M.keys cave
        maxXy = foldl' (xyBiMap max) minBound $ start : M.keys cave
    in fix2 start (Right cave) \loop xy e ->
        e >>= \m ->                                         -- shortcut on Left Cave, continue on Right Cave
            if | stop minXy maxXy m xy     -> Left m        -- out of the board => stop
               | occupied minXy maxXy m xy -> Right m       -- occupied => fallback
               | otherwise ->
                       fmap (M.insert xy True)              -- map remaining sand blocks (at rest)
                   . loop (xy + XY 1 1)                     -- ... down-right
                   . loop (xy + XY (-1) 1)                  -- ... down-left
                   . loop (xy + XY 0 1) $ Right m           -- ... down (start with it)

solve
    :: (XY -> XY -> Cave -> XY -> Bool)
    -> (XY -> XY -> Cave -> XY -> Bool)
    -> String -> Int
solve stop occupied s =
    let cave       = parseCave s
        caveFilled = fillCave stop occupied cave (XY 500 0)
    in subtract (M.size cave) case caveFilled of
        Left n  -> M.size n                                 -- sand blocks falling out of the map
        Right n -> M.size n                                 -- no space for sand blocks to be added

solve1 :: String -> Int
solve1 = solve
    (\_ (XY _ maxY) _ (XY _ y) -> y > maxY)                 -- stop     = when 1st block leaves the cave
    (\_ _ m xy -> xy `M.member` m)                          -- occupied = obstructed

solve2 :: String -> Int
solve2 = solve
    (\_ _ _ _ -> False)                                     -- stop     = never
    (\_ (XY _ maxY) m xy@(XY _ y) ->                        -- occupied = obstructed or reached the floor
        xy `M.member` m || y >= maxY + 2)
