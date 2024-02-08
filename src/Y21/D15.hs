module Y21.D15 where

import qualified Data.Vector.Generic as VG

import           Geom.XY
import           Imports
import qualified Pathfinder          as P
import qualified Vec2                as V
import           Vec2                (Vec2 (..))

type Risk   = Word8
type Score  = Word16

-- 11637
-- 13813
-- 21365
parse :: String -> Vec2 Risk
parse = V.fromList . fmap (fmap $ fromIntegral . digitToInt) . lines

neighbors :: WH -> XY -> [XYI]
neighbors wh@(XY w h) xy =
    [ xy2i wh nXy
    | d <- udlr
    , let nXy@(XY nx ny) = xy + d
    , nx >= 0 && ny >= 0 && nx < w && ny < h                            -- keep traversable ones only
    ]

at :: Vec2 Risk -> XYI -> Risk
at Vec2{vec} = (vec VG.!)

atTiled :: Vec2 Risk -> WH -> XYI -> Risk
atTiled Vec2{wh,vec} wht i =
    let (XY w h) = wh
        (XY x y) = i2xy wht i
    in    vec VG.! xy2i wh (XY (x `mod` w) (y `mod` h))                 -- risk
        & (+) (fromIntegral $ x `div` w + y `div` h)                    -- add tile value
        & pred & (`mod` 9) & succ                                       -- wrap around

{-# INLINE[1] config1 #-}
config1 :: Vec2 Risk -> P.Config XYI Word16
config1 v@Vec2{wh,vec} =
    P.Config
        { neighbors = neighbors wh . i2xy wh
        , cost      = \_ -> fromIntegral . at v
        , remaining = Nothing -- Just $ fromIntegral . distanceManhattan wh . i2xy wh -- NOTE: produces garbage
        , start     = 0
        , goal      = (== VG.length vec - 1)
        }

{-# INLINE[1] config2 #-}
config2 :: Vec2 Risk -> P.Config XYI Word16
config2 v@Vec2{wh,vec} =
    let wht = xyMap (*5) wh
    in P.Config
        { neighbors = neighbors wht . i2xy wht
        , cost      = \_ -> fromIntegral . atTiled v wht
        , remaining = Nothing -- Just $ fromIntegral . distanceManhattan wh . i2xy wh -- NOTE: produces garbage
        , start     = 0
        , goal      = (== 5 * 5 * VG.length vec - 1)
        }

solve1, solve2, solve1MS, solve2MS, solve1MI, solve2MI :: String -> Int
solve1   = (\v -> fromIntegral . fromJust . P.minScoreVec         (VG.length (vec v)) . config1 $ v) <$> parse
solve2   = (\v -> fromIntegral . fromJust . P.minScoreVec (5 * 5 * VG.length (vec v)) . config2 $ v) <$> parse
solve1MS =        fromIntegral . fromJust . P.minScore                                . config1      <$> parse
solve2MS =        fromIntegral . fromJust . P.minScore                                . config2      <$> parse
solve1MI =        fromIntegral . fromJust . P.minScoreInt                             . config1      <$> parse
solve2MI =        fromIntegral . fromJust . P.minScoreInt                             . config2      <$> parse
