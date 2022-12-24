module Y22.D14 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser
import           Util            (divvy2, fix2)
import           XY

type Cave = Map XY Bool

-- 498,4 -> 498,6 -> 496,6
-- 503,4 -> 502,4 -> 502,9 -> 494,9
traces :: Parser [[XY]]
traces =
    trace' `endBy` eol
  where
    trace' = xy `sepBy` padded (string "->")
    xy = XY <$> natural <* char ',' <*> natural

fillCave
    :: (Cave -> XY -> Bool)                         -- stopOnFallout
    -> (Cave -> XY -> Bool)                         -- occupied
    -> Cave                                         -- cave
    -> XY                                           -- startXy
    -> Either Cave Cave                             -- left = infinitely dropping, right = nowhere to place
fillCave stopOnFallout occupied cave startXy =
    fix2 startXy (Right cave) \loop xy e ->
        e >>= \m ->                                 -- shortcut on Left Cave, continue on Right Cave
            if | stopOnFallout m xy -> Left m       -- out of the board => stop
               | occupied m xy      -> Right m      -- occupied => fallback
               | otherwise ->
                     fmap (M.insert xy True)        -- map remaining sand blocks (at rest)
                   . loop (xy + XY 1 1)             -- ... down-right
                   . loop (xy + XY (-1) 1)          -- ... down-left
                   . loop (xy + XY 0 1) $ Right m   -- ... down (start with it)

solve :: (Int -> Cave -> XY -> Bool) -> (Int -> Cave -> XY -> Bool) -> String -> Int
solve stopOnFallout occupied s =
    let cave           = parseOrDie traces s
                            & concatMap (divvy2 1)
                            & concatMap (uncurry line2dots)
                            & foldl' (\m xy -> M.insert xy True m) M.empty
        startXy        = XY 500 0
        maxY           = foldl' max minBound . fmap getY $ startXy : M.keys cave
        caveFilled     = fillCave (stopOnFallout maxY) (occupied maxY) cave startXy
        caveFilledSize = M.size case caveFilled of Left n -> n; Right n -> n
    in caveFilledSize - M.size cave
  where
    line2dots ::  XY -> XY -> [XY]
    line2dots a b =
        let dx = signum $ b - a                     -- TODO assert |dx| = 1
            n  = abs (b - a) & \(XY x y) -> x + y
        in take (1+n) (iterate (+ dx) a)

solve1 :: String -> Int
solve1 = solve
    (\maxY _ (XY _ y) -> y > maxY)                  -- stopOnFallout = when falling through the floor
    (\_ m xy -> xy `M.member` m)                    -- occupied      = obstructed

solve2 :: String -> Int
solve2 = solve
    (\_ _ _ -> False)                               -- stopOnFallout = no
    (\maxY m xy@(XY _ y) ->                         -- occupied      = obstructed or reached the floor
        xy `M.member` m || y >= maxY + 2)
