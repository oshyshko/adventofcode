module XYZ where

import           Point

data XYZ = XYZ X Y Z deriving (Eq, Ord)
type X = Int
type Y = Int
type Z = Int

type WHD = XYZ
type W = Int
type H = Int
type D = Int

instance Show XYZ where
    show (XYZ x y z) =
        if x == y && y == z
            then show x
            else "(" <> show x <> "," <> show y <> "," <> show z <> ")"

instance Num XYZ where
    (XYZ x y z) + (XYZ a b c) = XYZ (x + a)    (y + b)    (z + c)
    (XYZ x y z) - (XYZ a b c) = XYZ (x - a)    (y - b)    (z - c)
    (XYZ x y z) * (XYZ a b c) = XYZ (x * a)    (y * b)    (z * c)
    abs           (XYZ x y z) = XYZ (abs x)    (abs y)    (abs z)
    negate        (XYZ x y z) = XYZ (negate x) (negate y) (negate z)
    signum        (XYZ x y z) = XYZ (signum x) (signum y) (signum z)
    fromInteger x             = let v = fromIntegral x in XYZ v v v

instance Point XYZ Int where
    map     = xyzMap
    fold    = xyzFold
    append  = xyzAppend
    foldMap = xyzFoldMap

xyzMap :: (Int -> Int) -> XYZ -> XYZ
xyzMap f (XYZ x y z) = XYZ (f x) (f y) (f z)

xyzFold :: (Int -> Int -> Int) -> XYZ -> Int
xyzFold f (XYZ x y z) = f z $ f x y

xyzAppend :: (Int -> Int -> Int) -> XYZ -> XYZ -> XYZ
xyzAppend f (XYZ x y z) (XYZ a b c) = XYZ (f x a) (f y b) (f z c)

xyzFoldMap :: (v -> v -> v) -> (Int -> v) -> XYZ -> v
xyzFoldMap f g (XYZ x y z) = (g x `f` g y) `f` g z
