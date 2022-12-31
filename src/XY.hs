module XY where

import           Point

data XY = XY Int Int deriving (Eq, Ord, Bounded)
type X = Int
type Y = Int

type WH = XY
type W = Int
type H = Int

type XYI = Int

instance Show XY where
    show (XY x y) =
        if x == y
            then show x
            else "(" <> show x <> "," <> show y <> ")"

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
    append  = xyAppend
    foldMap = xyFoldMap

getX :: XY -> Int
getX (XY x _) = x

getY :: XY -> Int
getY (XY _ y) = y

xyMap :: (Int -> Int) -> XY -> XY
xyMap f (XY x y) = XY (f x) (f y)

xyBimap ::  (Int -> Int -> Int) -> XY -> XY -> XY
xyBimap f (XY x y) (XY a b) = XY (f x a) (f y b)

xyFold :: (Int -> Int -> Int) -> XY -> Int
xyFold f (XY x y) = f x y

xyAppend :: (Int -> Int -> Int) -> XY -> XY -> XY
xyAppend f (XY x y) (XY a b) = XY (f x a) (f y b)

xyFoldMap :: (v -> v -> v) -> (Int -> v) -> XY -> v
xyFoldMap f g (XY x y) = g x `f` g y

i2xy :: WH -> XYI -> XY
i2xy (XY w _) i = XY (rem i w) (quot i w)

xy2i :: WH -> XY -> XYI
xy2i (XY w _) (XY x y) = y * w + x
