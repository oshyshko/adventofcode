module XYZ where

data XYZ  = XYZ Int Int Int deriving (Eq, Ord)
data Line = Line XYZ XYZ    deriving (Eq, Ord, Show)

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

xyzmap :: (Int -> Int) -> XYZ -> XYZ
xyzmap f (XYZ x y z) = XYZ (f x) (f y) (f z)

xyzfold :: (Int -> Int -> Int) -> XYZ -> Int
xyzfold f (XYZ x y z) = f z $ f x y

xyzzip :: (Int -> Int -> Int) -> XYZ -> XYZ -> XYZ
xyzzip f (XYZ ax ay az) (XYZ bx by bz) = XYZ (f ax bx) (f ay by) (f az bz)
