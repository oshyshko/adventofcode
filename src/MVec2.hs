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

{-# INLINE[1] read #-}
read :: (PrimMonad m, VUM.Unbox v) => MVec2 m v -> XY -> m v
read v@(MVec2 wh vec) xy
    | xy `notWithin` v  = error $ "Out of bounds: " ++ show xy ++ " for size " ++ show wh
    | otherwise         = VUM.read vec (xy2i wh xy)

{-# INLINE[1] readMaybe #-}
readMaybe :: MVec2 m v -> XY -> m (Maybe v)
readMaybe v@(MVec2 wh vec) xy
    | xy `notWithin` v  = pure Nothing
    | otherwise         = Just <$> VUM.read vec (xy2i wh xy)

{-# INLINE[1] readOr #-}
readOr :: (PrimMonad m, VUM.Unbox v) => v -> MVec2 m v -> XY -> m v
readOr orV v@(MVec2 wh vec) xy
    | xy `notWithin` v  = return orV
    | otherwise         = VUM.read vec (xy2i wh xy)

{-# INLINE[1] write #-}
write :: MVec2 m v -> XY -> v -> m ()
write MVec2{wh,vec} xy = VUM.write vec (xy2i wh xy)

replicate :: (PrimMonad m, VUM.Unbox v) => WH -> v -> m (MVec2 m v)
replicate wh@(XY w h) v = MVec2 wh <$> VUM.replicate (w * h) v

{-# INLINE[1] within #-}
{-# INLINE[1] notWithin #-}
within, notWithin :: XY -> MVec2 m v -> Bool
within    (XY x y) (MVec2 (XY w h) _) = x >= 0 && y >= 0 && x < w && y < h
notWithin (XY x y) (MVec2 (XY w h) _) = x < 0 || y < 0 || x >= w || y >= h
