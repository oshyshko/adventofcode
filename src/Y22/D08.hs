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
        in  [   [ beam v    y    (XY   0 (-1)) xy   -- up
                , beam v    x    (XY (-1)  0)  xy   -- left
                , beam v (h-y-1) (XY   0   1)  xy   -- down
                , beam v (w-x-1) (XY   1   0)  xy   -- right
                ]
            | y <- [0..h-1]
            , x <- [0..w-1]
            , let xy = XY x y ]
  where
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
    . filter or
    . withEveryTree \origin -> all (< origin)

solve2 :: String -> Int
solve2 =
      maximum
    . fmap product
    . withEveryTree (scenery 0)
  where
    scenery acc origin = \case
        [] -> acc
        (t:ts)
            | t < origin -> scenery (acc + 1) origin ts
            | otherwise  -> acc + 1
