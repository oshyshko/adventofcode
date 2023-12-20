module Geom.AXYZ where

data XYZ a = XYZ a a a deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative XYZ where
    pure a = XYZ a a a
    liftA2 f (XYZ x0 y0 z0) (XYZ x1 y1 z1) = XYZ (f x0 x1) (f y0 y1) (f z0 z1)

instance (Num a) => Num (XYZ a) where
    (+)           = liftA2 (+)
    (-)           = liftA2 (-)
    (*)           = liftA2 (*)
    abs           = fmap abs
    negate        = fmap negate
    signum        = fmap signum
    fromInteger x = XYZ v v v where v = fromIntegral x
