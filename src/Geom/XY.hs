module Geom.XY where

import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)

import           Geom.Point

-- NOTE: adding {-# UNPACK #-} to getX/getY doesn't reduce heap usage. Also in ghci.
data XY = XY
    { getX :: Int
    , getY :: Int
    } deriving (Eq, Ord, Bounded, Generic)

type X = Int
type Y = Int

type WH = XY
type W = Int
type H = Int

type XYI = Int

pattern U, D, L, R :: XY
pattern U = XY   0 (-1)
pattern D = XY   0   1
pattern L = XY (-1)  0
pattern R = XY   1   0

{-# INLINE[1] udlr #-}
udlr :: [XY]
udlr = [U,D,L,R]

instance Hashable XY

instance Show XY where
    show (XY x y)
        | x == y = show x
        | otherwise = "(" <> show x <> "," <> show y <> ")"

instance Num XY where
    (XY x y) + (XY a b) = XY (x + a)    (y + b)
    (XY x y) - (XY a b) = XY (x - a)    (y - b)
    (XY x y) * (XY a b) = XY (x * a)    (y * b)
    abs        (XY x y) = XY (abs x)    (abs y)
    negate     (XY x y) = XY (negate x) (negate y)
    signum     (XY x y) = XY (signum x) (signum y)
    fromInteger x       = let v = fromIntegral x in XY v v

instance Point XY Int where
    map     = xyMap
    fold    = xyFold
    zipWith = xyZipWith
    foldMap = xyFoldMap
    foldZip = xyFoldZip

xyMap :: (Int -> Int) -> XY -> XY
xyMap f (XY x y) = XY (f x) (f y)

xyBimap ::  (Int -> Int -> Int) -> XY -> XY -> XY
xyBimap f (XY x y) (XY a b) = XY (f x a) (f y b)

xyZipWith :: (Int -> Int -> Int) -> XY -> XY -> XY
xyZipWith f (XY x y) (XY a b) = XY (f x a) (f y b)

xyFold :: (Int -> Int -> Int) -> XY -> Int
xyFold f (XY x y) = f x y

xyFoldMap :: (v -> v -> v) -> (Int -> v) -> XY -> v
xyFoldMap f g (XY x y) = g x `f` g y

xyFoldZip :: (v -> v -> v) -> (Int -> Int -> v) -> XY -> XY -> v
xyFoldZip f z (XY x y) (XY a b) = f (z x a) (z y b)

i2xy :: WH -> XYI -> XY
i2xy (XY w _) i = XY (rem i w) (quot i w)

xy2i :: WH -> XY -> XYI
xy2i (XY w _) (XY x y) = y * w + x

xySwap :: XY -> XY
xySwap (XY x y) = XY y x

distanceManhattan :: XY -> XY -> Int
distanceManhattan = Geom.Point.distanceManhattan
