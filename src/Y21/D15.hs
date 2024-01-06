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

solve1, solve2, solve1MI, solve2MI, solve1MS, solve2MS :: String -> Int
solve1   = (\v@Vec2{wh,vec} ->                            fromJust $ P.minScoreMVector (        VG.length vec) (P.World (neighbors wh  . i2xy wh)  (\_ -> fromIntegral . at v)         ) 0 (        VG.length vec - 1)) <$> parse
solve2   = (\v@Vec2{wh,vec} -> let wht = xyMap (*5) wh in fromJust $ P.minScoreMVector (5 * 5 * VG.length vec) (P.World (neighbors wht . i2xy wht) (\_ -> fromIntegral . atTiled v wht)) 0 (5 * 5 * VG.length vec - 1)) <$> parse

solve1MI = (\v@Vec2{wh,vec} ->                            fromJust $ P.minScoreIntMap                          (P.World (neighbors wh  . i2xy wh)  (\_ -> fromIntegral . at v)         ) 0 (        VG.length vec - 1)) <$> parse
solve2MI = (\v@Vec2{wh,vec} -> let wht = xyMap (*5) wh in fromJust $ P.minScoreIntMap                          (P.World (neighbors wht . i2xy wht) (\_ -> fromIntegral . atTiled v wht)) 0 (5 * 5 * VG.length vec - 1)) <$> parse

solve1MS = (\v@Vec2{wh,vec} ->                            fromJust $ P.minScoreMap                             (P.World (neighbors wh  . i2xy wh)  (\_ -> fromIntegral . at v)         ) 0 (        VG.length vec - 1)) <$> parse
solve2MS = (\v@Vec2{wh,vec} -> let wht = xyMap (*5) wh in fromJust $ P.minScoreMap                             (P.World (neighbors wht . i2xy wht) (\_ -> fromIntegral . atTiled v wht)) 0 (5 * 5 * VG.length vec - 1)) <$> parse
