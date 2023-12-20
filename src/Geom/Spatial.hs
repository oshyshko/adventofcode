module Geom.Spatial where

import           Data.Foldable (minimumBy)
import           Data.Function (on, (&))
import           Data.List     (sortBy)

import           Geom.Range    (Range (..))
import qualified Geom.Range    as R
import           Geom.Point    (Point, distanceManhattan)
import           Geom.XY       (XY (..))
import           Geom.XYZ      (XYZ (..))

-- r = region (phantom), e.g. Range
-- p = coordinate, e.g. XY, XYZ
-- v = value, e.g. Bool, Int
data Tree r p v where
    Leaf  :: v                 -> Tree r p v
    Split :: p -> [Tree r p v] -> Tree r p v
    deriving (Eq, Show)

-- r = region, e.g. Range
-- p = coordinate, e.g. XY, XYZ, Int
class Region r p where
    center       :: r -> p
    intersection :: r -> r -> Maybe r
    bounds       :: r
    distance     :: p -> p -> Int
    corners      :: r -> [p]
    onCorner     :: r -> p -> Bool
    split        :: r -> p -> [r]
    splitLength  :: Int

instance Region (Range Int Int) Int where
    center                  = R.center
    intersection            = R.intersection
    bounds                  = rangeBounds
    distance                = distanceManhattan

    corners     (Range o s)   = [o, o + s]
    onCorner    (Range o s) x = o == x || x == (o + s)
    splitLength               = 2
    split       (Range o s) r = [Range o (r - o), Range r (o + s - r)]

instance Region (Range XY Int) XY where
    center                  = R.center
    intersection            = R.intersection
    bounds                  = rangeBounds
    distance                = distanceManhattan

    corners (Range (XY x y) (XY w h)) =
        XY <$> [x, x + w] <*>  [y, y + h]

    onCorner (Range o@(XY ox oy) s) (XY x y) =
        let (XY ox' oy') = o + s
        in     (x == ox || x == ox')
            && (y == oy || y == oy')

    splitLength = 4

    split (Range a@(XY ax ay) as) r@(XY rx ry) =
        let (XY bw bh) = r - a
            (XY pw ph) = a + as - r
        in  [ Range @XY (XY x y) (XY w h)
            | (x,w) <- [(ax,bw), (rx,pw)]
            , (y,h) <- [(ay,bh), (ry,ph)]
            ]

instance Region (Range XYZ Int) XYZ where
    center                  = R.center
    intersection            = R.intersection
    bounds                  = rangeBounds
    distance                = distanceManhattan

    corners (Range (XYZ x y z) (XYZ w h d)) =
        [ XYZ x' y' z'
        | x' <- [x, x + w]
        , y' <- [y, y + h]
        , z' <- [z, z + d]
        ]

    onCorner (Range a@(XYZ ax ay az) as) (XYZ x y z) =
        let (XYZ ax' ay' az') = a + as
        in     (ax == x || x == ax')
            && (ay == y || y == ay')
            && (az == z || z == az')

    splitLength = 8

    split (Range o@(XYZ ax ay az) s) r@(XYZ rx ry rz) =
        let (XYZ bw bh bd) = r - o
            (XYZ pw ph pd) = o + s - r
        in  [ Range @XYZ (XYZ x y z) (XYZ w h d)
            | (x,w) <- [(ax,bw), (rx,pw)]
            , (y,h) <- [(ay,bh), (ry,ph)]
            , (z,d) <- [(az,bd), (rz,pd)]
            ]

mkTree :: forall r p v. v -> Tree r p v
mkTree = Leaf

-- +---------+
-- |         |
-- |  +-.-+  |    <- outerCenter
-- |  |   |  |
-- |  I---+  |
-- O---------+
{-# INLINE bestSplitCenter #-}
bestSplitCenter :: forall r p . (Region r p) => r -> r -> p
bestSplitCenter a b =
    let aCenter = center @r @p a
    in    corners @r @p b
        & filter (not . onCorner @r @p a)
        & fmap (\p -> (distance @r @p aCenter p, p)) -- the most close to outer center
        & sortBy (compare `on` fst)
        & minimumBy (compare `on` fst)
        & snd

--      /+---++---Z  o + s
--    /+ |   ||   |
--   + |/+---+----+
--   |/|/+---++---+
--   |/| |   ||   |
--   | |/+---++---+
--   |/+----//--+
--   A----//---/
-- o
--
{-# INLINE set #-}
set :: forall r p v . (Region r p, Eq v, Eq r)
    => v -> r -> Tree r p v -> Tree r p v
set newValue =
    go (bounds @r @p)
  where
    {-# INLINE go #-}
    go :: r -> r -> Tree r p v -> Tree r p v
    go outer inner = \case
        t@(Leaf oldValue) ->
            if outer == inner || oldValue == newValue
                then Leaf newValue
                else go outer inner $ Split
                    (bestSplitCenter @r outer inner)
                    (replicate (splitLength @r @p) t)

        Split p os -> Split p (zipWith (update inner) os $ split @r @p outer p)

    {-# INLINE update #-}
    update :: r -> Tree r p v -> r -> Tree r p v
    update inner t s =
        intersection @r @p s inner
            & maybe t (\overlap -> go s overlap t)

{-# INLINE toList #-}
toList :: forall r p v. Region r p => Tree r p v -> [(r, v)]
toList =
    go (bounds @r @p)
  where
    {-# INLINE go #-}
    go :: r -> Tree r p v -> [(r, v)]
    go outer = \case
        Leaf v     -> [(outer, v)]
        Split r os -> concat $ zipWith go (split @r @p outer r) os

-- TODO remove?
{-# INLINE rangeBounds #-}
rangeBounds :: forall p c. (Point p c, Integral c, Bounded c) => Range p c
rangeBounds = Range
    (fromIntegral $ minBound @c `quot` 4)
    (fromIntegral $ maxBound @c `quot` 2)
