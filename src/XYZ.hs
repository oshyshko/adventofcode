module XYZ where

data XYZ = XYZ Int Int Int deriving (Eq, Ord, Show)

instance Num XYZ where
    (XYZ x y z) + (XYZ a b c) = XYZ (x + a)    (y + b)    (z + c)
    (XYZ x y z) - (XYZ a b c) = XYZ (x - a)    (y - b)    (z - c)
    (XYZ x y z) * (XYZ a b c) = XYZ (x * a)    (y * b)    (z * c)
    abs           (XYZ x y z) = XYZ (abs x)    (abs y)    (abs z)
    negate        (XYZ x y z) = XYZ (negate x) (negate y) (negate z)
    signum        (XYZ x y z) = XYZ (signum x) (signum y) (signum z)
    fromInteger x             = let v = fromIntegral x in XYZ v v v
