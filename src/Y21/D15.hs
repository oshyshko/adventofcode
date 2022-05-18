{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Y21.D15 where

import qualified Data.IntMap.Strict  as M
import qualified Data.IntPSQ         as Q
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU

import           Imports

type XY     = (Int, Int)
type WH     = XY
type XYI    = Int
type Risk   = Word8
type Score  = Int

-- 11637
-- 13813
-- 21365
parse :: String -> Vec2 Risk
parse s =
    let xs = lines s
    in Vec2
        { vecWh     = (length . head $ xs, length xs)
        , vecValues = V.fromList . fmap (fromIntegral . digitToInt) . concat $ xs
        }

findPath :: XYI -> XYI -> Vec2 Risk -> [XYI]
findPath start goal visit =
    go  (M.singleton start ())           -- closed
        (Q.singleton start maxBound ())  -- open
        M.empty                          -- this `cameFrom` (origin,score)
  where
    go :: IntMap () -> Q.IntPSQ Score () -> IntMap (XYI, Score) -> [XYI]
    go closed open cameFrom =
        let (mKpv, openEjected) = Q.alterMin (,Nothing) open        -- eject `current` from OPEN with the lowest F
        in case mKpv of
            Nothing -> error "No path"                              -- exghausted
            Just (current, _, _) ->
                if current == goal                                  -- goal reached?
                    then findWayBack cameFrom current
                    else
                          neighbors (i2xy (vecWh visit) current)    -- for each neighbor ...
                        & mapMaybe (\nXy -> do
                            nVisit <- atMaybe visit nXy             -- ... if it's traversable
                            let nXyi  = xy2i (vecWh visit) nXy
                                nCost = fromIntegral nVisit + maybe 0 snd (cameFrom M.!? current)
                            if     M.member nXyi closed             -- ... if not seen before
                                || Q.member nXyi open
                                then Nothing
                                else
                                      cameFrom M.!? nXyi            -- update ...
                                    & maybe
                                        (Just (nXyi, nCost))        -- ... if not seen before
                                        (\(_,oldCost) ->
                                            if nCost < oldCost      -- ... or if a better path found
                                                then Just (nXyi, nCost)
                                                else Nothing))
                        & foldl'                                    -- apply collected changes
                            (\(openAcc,cameFromAcc) (nXyi,nCost) ->
                                ( Q.insert nXyi nCost () openAcc
                                , M.insert nXyi (current,nCost) cameFromAcc))
                            (openEjected,cameFrom)
                        & uncurry (go (M.insert current () closed)) -- (go closed) open cameFrom

findWayBack :: IntMap (XYI,Score) -> XYI -> [XYI]
findWayBack cameFrom xy =
        cameFrom M.!? xy
    & maybe [] (\(p,_) ->
        if p == xy
            then error ("Can't point to self: " <> show p)
            else findWayBack cameFrom p)
    & (<> [xy])

neighbors :: XY -> [XY]
neighbors (x,y) = [(-1, 0), (1, 0), (0, -1), (0, 1)] <&> \(u,v) -> (x + u, y + v)

tileTimes :: Integral v => Int -> Vec2 v -> Vec2 v
tileTimes t Vec2{vecWh,vecValues} =
    let (w,h)        = vecWh
        wht@(wt, ht) = (w * t, h * t)
    in Vec2
        { vecWh     = wht
        , vecValues = V.fromList $ do
            y <- [0..(ht - 1)]
            x <- [0..(wt - 1)]
            pure $ (vecValues V.! xy2i vecWh (x `mod` w, y `mod` h))
                & (+) (fromIntegral $ x `div` w + y `div` h)
                & pred & (`mod` 9) & succ
        }

solve :: Vec2 Risk -> Int
solve visit@Vec2{vecWh,vecValues} =
    let start = 0
        goal  = xy2i vecWh (fst vecWh - 1, snd vecWh - 1)
    in findPath start goal visit
        & tail                                      -- remove starting position
        & fmap (fromIntegral . (vecValues V.!))
        & sum

solve1, solve2 :: String -> Int
solve1 = solve . parse
solve2 = solve . tileTimes 5 . parse

-- helpers
i2xy :: WH -> XYI -> XY
i2xy (w,h) i = (rem i w, quot i h)

xy2i :: WH -> XY -> XYI
xy2i (w,_) (x,y) = y * w + x

-- Vec2 stuff
data Vec2 v where
    Vec2 :: VU.Unbox v =>
        { vecWh     :: XY
        , vecValues :: VU.Vector v
        } -> Vec2 v

atMaybe :: Vec2 v -> XY -> Maybe v
atMaybe (Vec2 (w, h) v) (x, y) =
    if x < 0 || y < 0 || x >= w || y >= h
        then Nothing
        else Just $ v V.! (y * w + x)
