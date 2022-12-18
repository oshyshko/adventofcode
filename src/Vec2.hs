module Vec2 where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU

import           Imports
import           MVec2               (MVec2 (..))
import           XY

data Vec2 a where
    Vec2 :: (VU.Unbox a, Show a) =>
        { wh  :: XY
        , vec :: VU.Vector a
        } -> Vec2 a

deriving instance Eq a => Eq (Vec2 a)

instance (VU.Unbox a, Show a) => Show (Vec2 a) where
  show v@Vec2{vec} =
    let maxWidth = maximum . fmap (length . show) . V.toList $ vec
        showCell = printf ("%" ++ show maxWidth ++ "s") . show
    in  ("\n" <> )
      . concatMap ((++ "\n") . unwords . fmap showCell)
      . toList
      $ v

map :: (VU.Unbox a, VU.Unbox b, Show b) => (a -> b) -> Vec2 a -> Vec2 b
map f Vec2{wh,vec} = Vec2 wh (V.map f vec)

ifoldl' :: (a -> XY -> b -> a) -> a -> Vec2 b -> a
ifoldl' f acc Vec2{wh,vec} = V.ifoldl' (\a i b -> f a (i2xy wh i) b) acc vec

thaw :: (PrimMonad m) =>Vec2 v -> m (MVec2 m v)
thaw Vec2{wh,vec} = MVec2 wh <$> V.thaw vec

freeze :: (PrimMonad m, Show a) => MVec2 m a -> m (Vec2 a)
freeze MVec2{wh,vec} = Vec2 wh <$> V.freeze vec

toList :: VU.Unbox a => Vec2 a -> [[a]]
toList (Vec2 (XY w _) vec) = chunksOf w (VU.toList vec)

{-# INLINE[1] fromList #-}
fromList :: (VU.Unbox a, Show a) => [[a]] -> Vec2 a
fromList xs =
    -- TODO assert xs has equal lengths for all elements
    Vec2
        (XY (length . head $ xs) (length xs))
        (VU.fromList . concat $ xs)

{-# INLINE[1] (!) #-}
(!) :: Vec2 v -> XY -> v
(!) (Vec2 wh@(XY w h) v) xy@(XY x y)
    | x < 0 || y < 0 || x >= w || y >= h =
        error $ "Out of bounds: " ++ show xy ++ " for size " ++ show wh
    | otherwise = (V.!) v (xy2i wh xy)

{-# INLINE[1] atMaybe #-}
atMaybe :: Vec2 v -> XY -> Maybe v
atMaybe (Vec2 wh@(XY w h) v) xy@(XY x y)
    | x < 0 || y < 0 || x >= w || y >= h = Nothing
    | otherwise = Just $ (V.!) v (xy2i wh xy)

{-# INLINE[1] getOr #-}
getOr :: v -> Vec2 v -> XY -> v
getOr orV v xy = fromMaybe orV $ atMaybe v xy

{-# INLINE[1] findIndex #-}
findIndex :: (v -> Bool) -> Vec2 v -> Maybe XY
findIndex f Vec2{wh,vec} = V.findIndex f vec <&> i2xy wh
