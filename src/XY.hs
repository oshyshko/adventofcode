module XY where

data XY = XY Int Int deriving (Show, Eq, Ord)
type WH = XY

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

scale :: XY -> Int -> XY
scale (XY x y ) n = XY (x * n) (y * n)

i2xy :: WH -> Int -> XY
i2xy (XY w h) i = XY (rem i w) (quot i h)

xy2i :: WH -> XY -> Int
xy2i (XY w _) (XY x y) = y * w + x
