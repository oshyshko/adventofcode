module Geom.Range where

import           Geom.Point (Point)
import qualified Geom.Point as P

-- p = point, e.g. X, XY, XYZ
-- c = component, e.g. Int
data Range p c where
    Range :: (Point p c, Show p, Num c, Ord c, Integral c) =>
        { offset :: p
        , size   :: p
        } -> Range p c

instance Eq (Range p c) where
  (Range ao as) == (Range bo bs) = ao == bo && as == bs

instance Show (Range p c) where
  show (Range o s) = "(" <> show o <> ", " <> show s <> ")"

{-# INLINE contains #-}
contains :: forall p c. Point p c => Range p c -> Range p c -> Bool
(Range ao as) `contains` (Range bo bs) =
       P.foldMap @p @c (&&) (<= (0::c)) (ao - bo)
    && P.foldMap @p @c (&&) (>= (0::c)) (ao + as - (bo + bs))

{-# INLINE intersects #-}
intersects :: forall p c. (Point p c) => Range p c -> Range p c -> Bool
(Range ao as) `intersects` (Range bo bs) =
    let az = P.zipWith @p @c (+) ao as      -- offset + size = end
        bz = P.zipWith @p @c (+) bo bs
    in     P.foldZip @p @c (&&) (<) ao bz
        && P.foldZip @p @c (&&) (>) az bo

{-# INLINE intersection #-}
intersection :: forall p c. (Point p c) => Range p c -> Range p c -> Maybe (Range p c)
intersection (Range ao as) (Range bo bs) =
    let o = P.zipWith @p @c max ao bo
        s = P.zipWith @p @c min (ao + as) (bo + bs) - o
    in if P.foldMap @p @c (||) (<= (0::c)) s
        then Nothing
        else Just $ Range @p @c o s

center :: forall p c. Range p c -> p
center (Range o s) = o + P.map @p @c (`quot` (2 :: c)) s
