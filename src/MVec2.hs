module MVec2 where

import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           XY

type Index  = Int

data MVec2 m v where
    MVec2 :: (PrimMonad m, VUM.Unbox v) =>
        { wh  :: XY
        , vec :: VUM.MVector (PrimState m) v
        } -> MVec2 m v

{-# INLINE[1] atMaybe #-}
atMaybe :: MVec2 m v -> XY -> m (Maybe v)
atMaybe (MVec2 wh@(XY w h) v) xy@(XY x y) =
    if x < 0 || y < 0 || x >= w || y >= h
        then pure Nothing
        else Just <$> VUM.read v (xy2i wh xy)

{-# INLINE[1] readOr #-}
readOr :: PrimMonad m => v -> MVec2 m v -> XY -> m v
readOr orV v xy = fromMaybe orV <$> atMaybe v xy

{-# INLINE[1] write #-}
write :: MVec2 m v -> XY -> v -> m ()
write MVec2{wh,vec} xy = VUM.write vec (xy2i wh xy)

{-# INLINE[1] getOr #-} -- reduces allocations in Y15.D18: 532MB -> 486MB
getOr :: (PrimMonad m, VUM.Unbox v) => v -> MVec2 m v -> XY -> m v
getOr orV (MVec2 wh@(XY w h) vec) xy@(XY x y) =
    if x < 0 || y < 0 || x >= w || y >= h
        then return orV
        else VUM.read vec (xy2i wh xy)
