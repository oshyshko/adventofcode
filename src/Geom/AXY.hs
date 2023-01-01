module Geom.AXY where

import           Imports

data XY a   = XY   a a           deriving (Show, Functor, Foldable)
data Line a = Line (XY a) (XY a) deriving (Show, Functor, Foldable)

instance Applicative XY where
    pure a = XY a a
    liftA2 f (XY x0 y0) (XY x1 y1) = XY (f x0 x1) (f y0 y1)
