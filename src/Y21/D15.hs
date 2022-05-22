{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Y21.D15 where

import           Control.Monad.ST            (runST)
import qualified Data.IntMap.Strict          as M
import qualified Data.IntPSQ                 as Q
import qualified Data.STRef                  as S
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports

type XY     = (Int, Int)
type WH     = XY
type XYI    = Int
type Risk   = Word8
type Score  = Word16

minScore :: XYI -> XYI -> Vec2 Risk -> Score
minScore start goal Vec2{vecWh,vecValues} =
    (flip . flip fix)
        (Q.singleton start maxBound ())  -- open
        (M.singleton start 0)            -- xy2score
        \loop open xyi2score ->
            Q.alterMin (,Nothing) open & \case
                (Nothing, _) -> error "No path"                         -- exghausted ?
                (Just (current, _, _), openEjected) ->
                    let currentScore = xyi2score M.! current
                    in if current == goal                               -- goal reached?
                        then currentScore
                        else
                              neighbors vecWh (i2xy vecWh current)      -- for each neighbor
                            & foldl'                                    -- apply collected changes
                                (\(openAcc,xyi2scoreAcc) nXy ->
                                    let nXyi      = xy2i vecWh nXy
                                        nRisk     = vecValues V.! nXyi
                                        nScoreOld = M.findWithDefault maxBound nXyi xyi2score
                                        nScore    = fromIntegral nRisk + currentScore
                                    in if nScore < nScoreOld
                                        then ( Q.insert nXyi nScore () openAcc
                                             , M.insert nXyi nScore xyi2scoreAcc
                                             )
                                        else (openAcc,xyi2scoreAcc)
                                    )
                                (openEjected,xyi2score)
                            & uncurry loop

minScoreMU :: XYI -> XYI -> Vec2 Risk -> Score
minScoreMU start goal Vec2{vecWh,vecValues} = runST $ do
    open      <- S.newSTRef $ Q.singleton start (maxBound::Score) ()
    xyi2score <- VUM.replicate (V.length vecValues) (maxBound::Score)

    VUM.write xyi2score start 0

    fix \loop -> do
        S.readSTRef open <&> Q.alterMin (,Nothing) >>= \case
            (Nothing, _) -> error "No path"                             -- exghausted ?
            (Just (current, _, _), openEjected) -> do
                S.writeSTRef open openEjected
                currentScore <- VUM.read xyi2score current
                if current == goal                                      -- goal reached?
                    then return currentScore
                    else do                                             -- for each neighbor
                        forM_ (neighbors vecWh (i2xy vecWh current)) \nXy -> do
                            let nXyi   = xy2i vecWh nXy
                                nRisk = vecValues V.! nXyi
                                nScore = fromIntegral nRisk + currentScore
                            nScoreOld <- VUM.read xyi2score nXyi
                            when (nScore < nScoreOld) $ do              -- apply changes
                                S.modifySTRef' open (Q.insert nXyi nScore ())
                                VUM.write xyi2score nXyi nScore
                        loop

solve :: (XYI -> XYI -> Vec2 Risk -> Score) -> Vec2 Risk -> Int
solve minScoreF visit@Vec2{vecWh} =
    let (w,h) = vecWh
        start = 0
        goal  = xy2i vecWh (w - 1, h - 1)
    in fromIntegral $ minScoreF start goal visit

solve1, solve2, solve1MU, solve2MU :: String -> Int
solve1   = solve minScore                 . parse
solve2   = solve minScore   . tileTimes 5 . parse
solve1MU = solve minScoreMU               . parse
solve2MU = solve minScoreMU . tileTimes 5 . parse

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

neighbors :: WH -> XY -> [XY]
neighbors (w,h) (x,y) =
    [ nXy
    | (dx,dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)] -- deltas of 4 neighbors
    , let nXy@(nx,ny) = (x + dx, y + dy)
    , nx >= 0 && ny >= 0 && nx < w && ny < h        -- keep traversable ones only
    ]

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

-- Vec2 stuff
data Vec2 v where
    Vec2 :: VU.Unbox v =>
        { vecWh     :: XY
        , vecValues :: VU.Vector v
        } -> Vec2 v

i2xy :: WH -> XYI -> XY
i2xy (w,h) i = (rem i w, quot i h)

xy2i :: WH -> XY -> XYI
xy2i (w,_) (x,y) = y * w + x
