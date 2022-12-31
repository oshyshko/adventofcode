module Point where

-- p = point, e.g. XY, XYZ
-- c = component, e.g. Int
class (Num p, Show p, Ord p, Num c, Ord c, Show c) => Point p c where
    map     :: (c -> c) -> p -> p
    fold    :: (c -> c -> c) -> p -> c
    append  :: (c -> c -> c) -> p -> p -> p
    foldMap :: (v -> v -> v) -> (c -> v) -> p -> v

instance Point Int Int where
  map       f = f
  fold      _ = id
  append    f = f
  foldMap   _ = id

distanceManhattan :: (Point p c) => p -> p -> c
distanceManhattan a b = Point.fold (+) (abs $ a - b)
