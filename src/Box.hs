module Box where

import           Point   (Point)
import qualified Point

-- p = point, e.g. XY, XYZ
-- c = component, e.g. Int
data Box p c where
    Box :: (Point p c, Show p, Num c, Ord c, Integral c)
        => p -> p -> Box p c

instance Eq (Box p c) where
  (Box ao as) == (Box bo bs) = ao == bo && as == bs

instance Show (Box p c) where
  show (Box o s) = "(" <> show o <> ", " <> show s <> ")"

{-# INLINE contains #-}
contains :: forall p c. Point p c => Box p c -> Box p c -> Bool
(Box ao as) `contains` (Box bo bs) =
       Point.foldMap @p @c (&&) (<= (0::c)) (ao - bo)
    && Point.foldMap @p @c (&&) (>= (0::c)) (ao + as - (bo + bs))

{-# INLINE intersects #-}
intersects :: forall p c. (Point p c) => Box p c -> Box p c -> Bool
(Box ao as) `intersects` (Box bo bs) =
    let o = Point.append @p @c max ao bo
        s = Point.append @p @c min (ao + as) (bo + bs) - o
    in not $ Point.foldMap @p @c (||) (<= (0::c)) s

{-# INLINE intersection #-}
intersection :: forall p c. (Point p c) => Box p c -> Box p c -> Maybe (Box p c)
intersection (Box ao as) (Box bo bs) =
    let o = Point.append @p @c max ao bo
        s = Point.append @p @c min (ao + as) (bo + bs) - o
    in if Point.foldMap @p @c (||) (<= (0::c)) s
        -- w <= 0 || h <= 0 || d <= 0
        then Nothing
        else Just $ Box @p @c o s

center :: forall p c. Box p c -> p
center (Box o s) = o + Point.map @p @c (`quot` (2 :: c)) s

{-# INLINE offset #-}
offset :: Box p c -> p
offset (Box o _ ) = o

{-# INLINE size #-}
size :: Box p c -> p
size (Box _ s) = s
