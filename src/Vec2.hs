module Vec2 where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as VU

import           Geom.XY
import           Imports
import           Vec2M               (Vec2M (..))

data Vec2 a where
    Vec2 :: (VU.Unbox a, Show a) =>
        { wh  :: XY
        , vec :: VU.Vector a
        } -> Vec2 a

deriving instance Eq a => Eq (Vec2 a)

instance (VU.Unbox a, Show a) => Show (Vec2 a) where
    show v@Vec2{vec} =
        let maxWidth = maximum . fmap (length . show) . G.toList $ vec
            showCell = printf ("%" ++ show maxWidth ++ "s") . show
        in  ("\n" <> )
          . concatMap ((++ "\n") . unwords . fmap showCell)
          . toList
          $ v

showBits :: Vec2 Bit -> String
showBits =
    concatMap ((++ "\n") . unwords . fmap showBit) . toList
  where
    showBit = \case
        Bit True  -> "#"
        Bit False -> "."

map :: (VU.Unbox a, VU.Unbox b, Show b) => (a -> b) -> Vec2 a -> Vec2 b
map f Vec2{wh,vec} = Vec2 wh (G.map f vec)

foldl' :: (a -> b -> a) -> a -> Vec2 b -> a
foldl' f acc Vec2{vec} = G.foldl' f acc vec

ifoldl' :: (a -> XY -> b -> a) -> a -> Vec2 b -> a
ifoldl' f acc Vec2{wh,vec} = G.ifoldl' (\a i b -> f a (i2xy wh i) b) acc vec

thaw :: (PrimMonad m) =>Vec2 v -> m (Vec2M m v)
thaw Vec2{wh,vec} = Vec2M wh <$> G.thaw vec

freeze :: (PrimMonad m, Show a) => Vec2M m a -> m (Vec2 a)
freeze Vec2M{wh,vec} = Vec2 wh <$> G.freeze vec

toList :: VU.Unbox a => Vec2 a -> [[a]]
toList (Vec2 (XY w _) vec) = chunksOf w (VU.toList vec)

fromList :: (VU.Unbox a, Show a) => [[a]] -> Vec2 a
fromList xs =
    -- TODO assert xs has equal lengths for all elements
    Vec2
        (XY (length . head $ xs) (length xs))
        (VU.fromList . concat $ xs)

generate :: (VU.Unbox a, Show a) => WH -> (XY -> a) -> Vec2 a
generate wh@(XY w h) f = Vec2 {wh, vec = VU.generate (w * h) (f . i2xy wh)}

{-# INLINE[1] (!) #-}
(!) :: Vec2 v -> XY -> v
(!) v@(Vec2 wh vec) xy
    | xy `notWithin` v  = error $ "Out of bounds: " ++ show xy ++ " for size " ++ show wh
    | otherwise         = (G.!) vec (xy2i wh xy)

{-# INLINE[1] (!?) #-}
(!?) :: Vec2 v -> XY -> Maybe v
(!?) v@(Vec2 wh vec) xy
    | xy `notWithin` v  = Nothing
    | otherwise         = Just $ (G.!) vec (xy2i wh xy)

{-# INLINE[1] getOr #-}
getOr :: v -> Vec2 v -> XY -> v
getOr orV v xy = fromMaybe orV $ v !? xy

{-# INLINE[1] findIndex #-}
findIndex :: (v -> Bool) -> Vec2 v -> Maybe XY
findIndex f Vec2{wh,vec} = G.findIndex f vec <&> i2xy wh

{-# INLINE[1] within #-}
{-# INLINE[1] notWithin #-}
within, notWithin :: XY -> Vec2 v -> Bool
within    (XY x y) (Vec2 (XY w h) _) = x >= 0 && y >= 0 && x < w && y < h
notWithin (XY x y) (Vec2 (XY w h) _) = x < 0 || y < 0 || x >= w || y >= h
