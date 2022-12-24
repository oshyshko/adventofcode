module XY where

data XY = XY Int Int deriving (Eq, Ord, Bounded)
type WH = XY
type XYI = Int

instance Show XY where
    show (XY x y) = "(" ++ show x ++ "," ++ show y ++ ")"

data Line = Line XY XY deriving (Show, Eq, Ord)

instance Num XY where
    (XY x y) + (XY a b) = XY (x + a)    (y + b)
    (XY x y) - (XY a b) = XY (x - a)    (y - b)
    (XY x y) * (XY a b) = XY (x * a)    (y * b)
    abs        (XY x y) = XY (abs x)    (abs y)
    negate     (XY x y) = XY (negate x) (negate y)
    signum     (XY x y) = XY (signum x) (signum y)
    fromInteger x       = let v = fromIntegral x in XY v v

getX :: XY -> Int
getX (XY x _) = x

getY :: XY -> Int
getY (XY _ y) = y

xyMap ::  (Int -> Int) -> XY -> XY
xyMap f (XY x y) = XY (f x) (f y)

xyBiMap ::  (Int -> Int -> Int) -> XY -> XY -> XY
xyBiMap f (XY x y) (XY a b)  = XY (f x a) (f y b)

i2xy :: WH -> Int -> XY
i2xy (XY w _) i = XY (rem i w) (quot i w)

xy2i :: WH -> XY -> Int
xy2i (XY w _) (XY x y) = y * w + x
