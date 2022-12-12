module Vec2 where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU

import           Imports
import           XY

type Index  = Int

data Vec2 a where
    Vec2 :: (VU.Unbox a, Show a) =>
        { wh  :: XY
        , vec :: VU.Vector a
        } -> Vec2 a

deriving instance Eq a => Eq (Vec2 a)

instance Show (Vec2 a) where
  show v@Vec2{} = ("\n" <> ) . intercalate "\n" $ fmap (concatMap show) . toList $ v

toList :: VU.Unbox a => Vec2 a -> [[a]]
toList (Vec2 (XY w _) vec) = chunksOf w (VU.toList vec)

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
