module Y22.D12 where

import qualified Data.Vector.Generic as VG

import           Geom.XY
import           Imports
import qualified Pathfinder          as P
import qualified Vec2                as V
import           Vec2                (Vec2 (..))

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
        | isAsciiLower h = fromIntegral $ ord h - ord 'a'
        | 'S' == h       = cost 'a'
        | 'E' == h       = cost 'z'
        | otherwise      = error $ "Unexpected height: " ++ show h

{-# INLINE[1] minScore #-}
minScore :: Vec2 Height -> XY -> XY -> Maybe Int
minScore Vec2{wh,vec} start goal =
    P.minScoreVec
        (VG.length vec)
        P.Config
            { neighbors = neighbors . i2xy wh
            , cost      = \_ _ -> 1
            , remaining = Nothing
            , start     = xy2i wh start
            , goal      = (== xy2i wh goal)
            }
  where
    -- TODO consider reducing value lookups by merging 'at' with 'neighbors`
    neighbors :: XY -> [XYI]
    neighbors p =
        [ nXyi
        | d <- udlr
        , let nXy@(XY nx ny) = p + d
        , nx >= 0 && ny >= 0 && nx < getX wh && ny < getY wh
        , let nXyi = xy2i wh nXy
        , (vec VG.! nXyi - 1) <= vec VG.! xy2i wh p
        ]

solve1 :: String -> Int
solve1 s =
    parseMapStartEnd s & \(v,start,goal) ->
        fromJust $ minScore v start goal

solve2 :: String -> Int
solve2 s =
      parseMapStartEnd s
    & \(v@Vec2{wh,vec},_,goal) ->
        VG.ifoldl' (\a i b -> if b == 0 then i:a else a) [] vec   -- starting points
    & mapMaybe (\start -> minScore v (i2xy wh start) goal)
    & minimum
