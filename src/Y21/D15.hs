module Y21.D15 where

import qualified Data.Vector.Generic as VG

import           Imports
import qualified Pathfinder
import qualified Vec2                as V
import           Vec2                (Vec2 (..))
import           XY

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
    | d <- [XY 0 (-1), XY 0 1, XY (-1) 0, XY 1 0]
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

solve1, solve2, solve1IM, solve2IM :: String -> Int
solve1   = (\v@Vec2{wh,vec} ->                            fromJust $ Pathfinder.minScoreMVector (        VG.length vec) (neighbors wh  . i2xy wh)  (at v)          fromIntegral 0 (        VG.length vec - 1)) <$> parse
solve2   = (\v@Vec2{wh,vec} -> let wht = xyMap (*5) wh in fromJust $ Pathfinder.minScoreMVector (5 * 5 * VG.length vec) (neighbors wht . i2xy wht) (atTiled v wht) fromIntegral 0 (5 * 5 * VG.length vec - 1)) <$> parse
solve1IM = (\v@Vec2{wh,vec} ->                            fromJust $ Pathfinder.minScoreIntMap                          (neighbors wh  . i2xy wh)  (at v)          fromIntegral 0 (        VG.length vec - 1)) <$> parse
solve2IM = (\v@Vec2{wh,vec} -> let wht = xyMap (*5) wh in fromJust $ Pathfinder.minScoreIntMap                          (neighbors wht . i2xy wht) (atTiled v wht) fromIntegral 0 (5 * 5 * VG.length vec - 1)) <$> parse
