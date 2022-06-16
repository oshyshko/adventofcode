module Vec2 where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU

import           Imports
import           XY

type Index  = Int

data Vec2 v where
    Vec2 :: VU.Unbox v =>
        { wh  :: XY
        , vec :: VU.Vector v
        } -> Vec2 v

{-# INLINE[1] atMaybe #-}
atMaybe :: Vec2 v -> XY -> Maybe v
atMaybe (Vec2 wh@(XY w h) v) xy@(XY x y) =
    if x < 0 || y < 0 || x >= w || y >= h
        then Nothing
        else Just $ (V.!) v (xy2i wh xy)

{-# INLINE[1] getOr #-}
getOr :: v -> Vec2 v -> XY -> v
getOr orV v xy = fromMaybe orV $ atMaybe v xy
