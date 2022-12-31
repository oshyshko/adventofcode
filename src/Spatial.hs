module Spatial where

import           Box     (Box (..), center, intersection)
import           Imports
import           Point   (Point)
import qualified Point
import           XY      (XY (..))
import           XYZ     (XYZ (..))

-- p = coordinate, e.g. XY, XYZ
-- c = component, e.g. Int
-- v = value, e.g. Bool, Int
data Tree p c v where
    Leaf  :: v                 -> Tree p c v
    Split :: p -> [Tree p c v] -> Tree p c v
    deriving (Eq, Show)

class (Point p c, Show p, Ord c, Integral c) => Spatial p c where
    children    :: Int
    corners     :: Box p c -> [p]
    onCorner    :: Box p c -> p -> Bool
    split       :: Box p c -> p -> [Box p c]

instance Spatial Int Int where
    children                = 2
    corners     (Box o s)   = [o, o + s]
    onCorner    (Box o s) x = o == x || x == (o + s)
    split       (Box o s) r = [Box o (r - o), Box r (o + s - r)]

instance Spatial XY Int where
    children = 4

    corners (Box (XY x y) (XY w h)) =
        XY <$> [x, x + w] <*>  [y, y + h]

    onCorner (Box o@(XY x y) s) (XY a b) =
        let (XY x' y') = o + s
        in     (x == a || a == x')
            && (y == b || b == y')

    split (Box a@(XY ax ay) as) r@(XY rx ry) =
        let (XY bw bh) = r - a
            (XY pw ph) = a + as - r
        in  [ Box @XY @Int (XY x y) (XY w h)
            | (x,w) <- [(ax,bw), (rx,pw)]
            , (y,h) <- [(ay,bh), (ry,ph)]
            ]

instance Spatial XYZ Int where
    children = 8

    corners (Box (XYZ x y z) (XYZ w h d)) =
        [ XYZ x' y' z'
        | x' <- [x, x + w]
        , y' <- [y, y + h]
        , z' <- [z, z + d]
        ]

    onCorner (Box a@(XYZ ax ay az) as) (XYZ x y z) =
        let (XYZ ax' ay' az') = a + as
        in     (ax == x || x == ax')
            && (ay == y || y == ay')
            && (az == z || z == az')

    split (Box o@(XYZ ax ay az) s) r@(XYZ rx ry rz) =
        let (XYZ bw bh bd) = r - o
            (XYZ pw ph pd) = o + s - r
        in  [ Box @XYZ @Int (XYZ x y z) (XYZ w h d)
            | (x,w) <- [(ax,bw), (rx,pw)]
            , (y,h) <- [(ay,bh), (ry,ph)]
            , (z,d) <- [(az,bd), (rz,pd)]
            ]

mkTree :: forall p c v. v -> Tree p c v
mkTree = Leaf

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
set :: forall p c v . (Spatial p c, Eq v)
    => v -> Box p c -> Tree p c v -> Tree p c v               -- new-value, new-box, source
set newValue =
    go (bounds @p @c)
  where
    {-# INLINE go #-}
    go :: Box p c -> Box p c -> Tree p c v -> Tree p c v
    go outer inner t = case t of
        Leaf oldValue ->
            if outer == inner || oldValue == newValue
                then Leaf newValue
                else go outer inner $ Split
                    (bestSplitCenter outer inner)
                    (replicate (children @p @c) t)

        Split p os -> Split p (zipWith (update inner) os $ split @p @c outer p)

    {-# INLINE update #-}
    update :: Box p c -> Tree p c v -> Box p c -> Tree p c v
    update inner t s =
        intersection @p @c s inner
            & maybe t (\overlap -> go s overlap t)

-- +---------+
-- |         |
-- |  +-.-+  |    <- outerCenter
-- |  |   |  |
-- |  I---+  |
-- O---------+
{-# INLINE bestSplitCenter #-}
bestSplitCenter :: forall p c . (Spatial p c) => Box p c -> Box p c -> p
bestSplitCenter a b =
    let aCenter = center @p @c a
    in    corners @p @c b
        & filter (not . onCorner @p @c a)
        & fmap (\p -> (Point.distanceManhattan @p @c aCenter p, p)) -- the most close to outer center
        & sortBy (compare `on` fst)
        & minimumBy (compare `on` fst)
        & snd

{-# INLINE toList #-}
toList :: forall p c v. Spatial p c => Tree p c v -> [(v, Box p c)]
toList =
    go (bounds @p @c)
  where
    {-# INLINE go #-}
    go :: Box p c -> Tree p c v -> [(v, Box p c)]
    go outer = \case
        Leaf v     -> [(v, outer)]
        Split r os -> concat $ zipWith go (split @p @c outer r) os

{-# INLINE bounds #-}
bounds :: forall p c. (Point p c, Integral c) => Box p c
bounds = Box
    (fromIntegral $ minBound @Int `quot` 4)
    (fromIntegral $ maxBound @Int `quot` 2)
