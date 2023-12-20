module Geom.Point where

-- p = point, e.g. X, XY, XYZ
-- c = component, e.g. Int
class (Num p, Ord p, Show p, Num c, Ord c, Show c) => Point p c where
    map     :: (c -> c)                       -> p      -> p
    zipWith :: (c -> c -> c)                  -> p -> p -> p
    fold    :: (c -> c -> c)                  -> p      -> c
    foldMap :: (v -> v -> v) -> (c -> v)      -> p      -> v
    foldZip :: (v -> v -> v) -> (c -> c -> v) -> p -> p -> v

-- TODO move to X.hs? (requires to deal with orphan instance for Int)
instance Point Int Int where
  map       f = f
  zipWith   f = f
  fold      _ = id
  foldMap   _ = id
  foldZip _ f = f

distanceManhattan :: (Point p c) => p -> p -> c
distanceManhattan a b = fold (+) (abs $ a - b)
