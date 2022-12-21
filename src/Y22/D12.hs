module Y22.D12 where

import qualified Data.Vector.Generic as VG

import           Imports
import qualified Pathfinder
import qualified Vec2                as V
import           Vec2                (Vec2 (..))
import           XY

type Height = Int8

parseMapStartEnd :: String -> (Vec2 Height, XY, XY)
parseMapStartEnd =
      (\v ->
        ( V.map (cost . chr) v
        , fromJust $ V.findIndex (== ord 'S') v
        , fromJust $ V.findIndex (== ord 'E') v
        ))
    . V.fromList
    . fmap (fmap ord)
    . lines
  where
    cost :: Char -> Height
    cost h
        | 'a' <= h && h <= 'z' = fromIntegral $ ord h - ord 'a'
        | 'S' == h             = cost 'a'
        | 'E' == h             = cost 'z'
        | otherwise            = error $ "Unexpected height: " ++ show h

minScore :: Vec2 Height -> XY -> XY -> Maybe Int
minScore Vec2{wh,vec} start goal =
    Pathfinder.minScoreMVector (VG.length vec) (neighbors . i2xy wh) at (const 1) (xy2i wh start) (xy2i wh goal)
  where
    -- TODO consider reducing value lookups by merging 'at' with 'neighbors`
    at :: XYI -> Height
    at = (vec VG.!)
    neighbors :: XY -> [XYI]
    neighbors xy =
        [ nXyi
        | d <- [XY 0 (-1), XY 0 1, XY (-1) 0, XY 1 0]
        , let nXy@(XY nx ny) = xy + d
        , let (XY w h ) = wh
        , nx >= 0 && ny >= 0 && nx < w && ny < h
        , let nXyi = xy2i wh nXy
        , (vec VG.! nXyi - 1) <= vec VG.! xy2i wh xy
        ]

solve1 :: String -> Int
solve1 s =
    parseMapStartEnd s & \(v,start,goal) ->
        fromJust $ minScore v start goal

solve2 :: String -> Int
solve2 s =
    parseMapStartEnd s & \(v@Vec2{wh,vec},_,goal) ->
          VG.ifoldl' (\a i b -> if b == 0 then i:a else a) [] vec   -- starting points
        & mapMaybe (\start -> minScore v (i2xy wh start) goal)
        & minimum