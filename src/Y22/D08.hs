module Y22.D08 where

import           Imports
import qualified Vec2     as V
import           Vec2     (Vec2 (..))
import           XY

type Height = Int8

withEveryTree :: forall a. (Height -> [Height] -> a) -> String -> [[a]]
withEveryTree foldBeamFn s =
      lines s
    & fmap (fmap $ fromIntegral @_ @Height . digitToInt)
    & V.fromList
    & \v@Vec2{wh} ->
        let (XY w h) = wh
        in  [   [ beam v    y    up    xy
                , beam v    x    left  xy
                , beam v (h-y-1) down  xy
                , beam v (w-x-1) right xy
                ]
            | y <- [0..h-1]
            , x <- [0..w-1]
            , let xy = XY x y ]
  where
    up, down, left, right :: XY
    up    = XY   0 (-1)
    down  = XY   0   1
    left  = XY (-1)  0
    right = XY   1   0

    beam :: Vec2 Height -> Int -> XY -> XY -> a
    beam v n dxy sxy =
          iterate (+ dxy) sxy
        & drop 1
        & take n
        & fmap (v V.!)
        & foldBeamFn (v V.! sxy)

solve1 :: String -> Int
solve1 =
      length
    . filter id
    . fmap (foldl' (||) False)
    . withEveryTree (unobstructed False)
  where
    unobstructed c origin = \case
        []     -> True
        (t:ts) -> (t < origin) && unobstructed c origin ts

solve2 :: String -> Int
solve2 =
      maximum
    . fmap (foldl' (*) 1)
    . withEveryTree (scenery 0)
  where
    scenery c origin = \case
        []     -> c
        (t:ts) -> if t < origin then scenery (c + 1) origin ts else c + 1
