module Vec2M where

import qualified Data.Vector.Unboxed.Mutable as VUM

import           Geom.XY
import           Imports

type Index  = Int

data Vec2M m v where
    Vec2M :: (PrimMonad m, VUM.Unbox v) =>
        { wh  :: XY
        , vec :: VUM.MVector (PrimState m) v
        } -> Vec2M m v

{-# INLINE[1] read #-}
read :: (PrimMonad m, VUM.Unbox v) => Vec2M m v -> XY -> m v
read v@(Vec2M wh vec) xy
    | xy `notWithin` v  = error $ "Out of bounds: " ++ show xy ++ " for size " ++ show wh
    | otherwise         = VUM.read vec (xy2i wh xy)

{-# INLINE[1] readMaybe #-}
readMaybe :: Vec2M m v -> XY -> m (Maybe v)
readMaybe v@(Vec2M wh vec) xy
    | xy `notWithin` v  = pure Nothing
    | otherwise         = Just <$> VUM.read vec (xy2i wh xy)

{-# INLINE[1] readOr #-}
readOr :: (PrimMonad m, VUM.Unbox v) => v -> Vec2M m v -> XY -> m v
readOr orV v@(Vec2M wh vec) xy
    | xy `notWithin` v  = return orV
    | otherwise         = VUM.read vec (xy2i wh xy)

{-# INLINE[1] write #-}
write :: Vec2M m v -> XY -> v -> m ()
write Vec2M{wh,vec} xy = VUM.write vec (xy2i wh xy)

replicate :: (PrimMonad m, VUM.Unbox v) => WH -> v -> m (Vec2M m v)
replicate wh@(XY w h) v = Vec2M wh <$> VUM.replicate (w * h) v

{-# INLINE[1] within #-}
{-# INLINE[1] notWithin #-}
within, notWithin :: XY -> Vec2M m v -> Bool
within    (XY x y) (Vec2M (XY w h) _) = x >= 0 && y >= 0 && x < w && y < h
notWithin (XY x y) (Vec2M (XY w h) _) = x < 0 || y < 0 || x >= w || y >= h

foldl' :: (a -> b -> a) -> a -> Vec2M m b -> m a
foldl' f acc Vec2M{vec} = VUM.foldl' f acc vec
