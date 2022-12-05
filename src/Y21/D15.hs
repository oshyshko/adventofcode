module Y21.D15 where

import           Control.Monad.ST            (runST)
import qualified Data.IntMap.Strict          as M
import qualified Data.IntPSQ                 as Q
import qualified Data.STRef                  as S
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           Util
import           Vec2
import           XY

type Risk   = Word8
type Score  = Word16

minScore :: Index -> Index -> Vec2 Risk -> Score
minScore start goal Vec2{wh,vec} =
    fix2
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
                              neighbors wh (i2xy wh current)            -- for each neighbor
                            & foldl'                                    -- apply collected changes
                                (\(openAcc,xyi2scoreAcc) nXy ->
                                    let nXyi      = xy2i wh nXy
                                        nRisk     = vec V.! nXyi
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

minScoreMU :: Index -> Index -> Vec2 Risk -> Score
minScoreMU start goal Vec2{wh,vec} = runST $ do
    open      <- S.newSTRef $ Q.singleton start (maxBound::Score) ()
    xyi2score <- VUM.replicate (V.length vec) (maxBound::Score)

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
                        forM_ (neighbors wh (i2xy wh current)) \nXy -> do
                            let nXyi   = xy2i wh nXy
                                nRisk = vec V.! nXyi
                                nScore = fromIntegral nRisk + currentScore
                            nScoreOld <- VUM.read xyi2score nXyi
                            when (nScore < nScoreOld) $ do              -- apply changes
                                S.modifySTRef' open (Q.insert nXyi nScore ())
                                VUM.write xyi2score nXyi nScore
                        loop

solve :: (Index -> Index -> Vec2 Risk -> Score) -> Vec2 Risk -> Int
solve minScoreF visit@Vec2{wh} =
    let start = 0
        goal  = xy2i wh $ wh - 1
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
        { wh  = XY (length . head $ xs) (length xs)
        , vec = V.fromList . fmap (fromIntegral . digitToInt) . concat $ xs
        }

neighbors :: WH -> XY -> [XY]
neighbors (XY w h) xy =
    [ nXy
    | d <- [XY (-1) 0, XY 1 0, XY 0 (-1), XY 0 1]   -- deltas of 4 neighbors
    , let nXy@(XY nx ny) = xy + d
    , nx >= 0 && ny >= 0 && nx < w && ny < h        -- keep traversable ones only
    ]

tileTimes :: Integral v => Int -> Vec2 v -> Vec2 v
tileTimes t Vec2{wh,vec} =
    let (XY w h)       = wh
        wht@(XY wt ht) = scale wh t
    in Vec2
        { wh  = wht
        , vec = V.fromList $ do
            y <- [0..(ht - 1)]
            x <- [0..(wt - 1)]
            pure $ (vec V.! xy2i wh (XY (x `mod` w) (y `mod` h)))
                & (+) (fromIntegral $ x `div` w + y `div` h)
                & pred & (`mod` 9) & succ
        }
