module Geom.Box where

import           Geom.Point (Point)
import qualified Geom.Point as P

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
       P.foldMap @p @c (&&) (<= (0::c)) (ao - bo)
    && P.foldMap @p @c (&&) (>= (0::c)) (ao + as - (bo + bs))

{-# INLINE intersects #-}
intersects :: forall p c. (Point p c) => Box p c -> Box p c -> Bool
(Box ao as) `intersects` (Box bo bs) =
    let o = P.append @p @c max ao bo
        s = P.append @p @c min (ao + as) (bo + bs) - o
    in not $ P.foldMap @p @c (||) (<= (0::c)) s

{-# INLINE intersection #-}
intersection :: forall p c. (Point p c) => Box p c -> Box p c -> Maybe (Box p c)
intersection (Box ao as) (Box bo bs) =
    let o = P.append @p @c max ao bo
        s = P.append @p @c min (ao + as) (bo + bs) - o
    in if P.foldMap @p @c (||) (<= (0::c)) s
        -- w <= 0 || h <= 0 || d <= 0
        then Nothing
        else Just $ Box @p @c o s

center :: forall p c. Box p c -> p
center (Box o s) = o + P.map @p @c (`quot` (2 :: c)) s

{-# INLINE offset #-}
offset :: Box p c -> p
offset (Box o _ ) = o

{-# INLINE size #-}
size :: Box p c -> p
size (Box _ s) = s
