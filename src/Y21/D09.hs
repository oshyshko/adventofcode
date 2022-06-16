module Y21.D09 where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set            as S

import           Imports
import           XY

type Height    = Char
data HeightMap = HeightMap XY (VU.Vector Height)    -- wh values

parse :: String -> HeightMap
parse s =
    let xs = lines s
        wh = XY (length . head $ xs) (length xs)
        v  = VU.fromList . concat $ xs
    in HeightMap wh v

neighbors :: [XY]
neighbors = [XY (-1) 0, XY 1 0, XY 0 (-1), XY 0 1]

atMaybe :: HeightMap -> XY -> Maybe Height      -- 9 or out of bounds = Nothing
atMaybe (HeightMap (XY w h) v) (XY x y) =
    if x < 0 || y < 0 || x >= w || y >= h
        then Nothing
        else (V.!) v (y * w + x) & \height ->
            if height == '9'
                then Nothing
                else Just height

lowPoint :: HeightMap -> XY -> Bool
lowPoint hm xy =
    atMaybe hm xy & maybe False \c ->
         all (maybe True (c <) . atMaybe hm . (+ xy)) neighbors

lowPoints :: HeightMap -> [(XY, Height)]
lowPoints hm@(HeightMap (XY w _) v) =
    V.ifoldl' f [] v
  where
    f ps i value =
        let xy = XY (rem i w) (quot i w)
        in if lowPoint hm xy then (xy, value):ps else ps

basins :: HeightMap -> [Set XY]
basins hm =
    flood S.empty . fst <$> lowPoints hm
  where
    flood :: Set XY -> XY -> Set XY
    flood s xy
        | S.member xy s = s
        | otherwise = atMaybe hm xy & \case
            Nothing -> s
            Just _  -> foldl' (\ss -> flood ss . (+ xy)) (S.insert xy s) neighbors

solve1, solve2 :: String -> Int
solve1 = sum . fmap ((+1) . digitToInt . snd) . lowPoints . parse
solve2 = product . take 3 . sortBy (flip compare) . fmap S.size . basins . parse
