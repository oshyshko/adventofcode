module Y21.D09 where

import qualified Data.Set as S
import           Geom.XY
import           Imports
import qualified Vec2     as V
import           Vec2     (Vec2 (..))

type Height = Char

parse :: String -> Vec2 Height
parse = V.fromList . lines

neighbors :: [XY]
neighbors = [XY (-1) 0, XY 1 0, XY 0 (-1), XY 0 1]

atMaybe :: Vec2 Height -> XY -> Maybe Height
atMaybe v xy =
    V.atMaybe v xy >>= \h ->
        if h == '9' then Nothing else Just h

lowPoint :: Vec2 Height -> XY -> Bool
lowPoint v xy =
    atMaybe v xy & maybe False \c ->
        all (maybe True (c <) . atMaybe v . (+ xy)) neighbors

lowPoints :: Vec2 Height -> [(XY, Height)]
lowPoints v =
    V.ifoldl' f [] v
  where
    f ps xy value = if lowPoint v xy then (xy, value):ps else ps

basins :: Vec2 Height -> [Set XY]
basins v =
    flood S.empty . fst <$> lowPoints v
  where
    flood :: Set XY -> XY -> Set XY
    flood s xy
        | S.member xy s = s
        | otherwise = atMaybe v xy & \case
            Nothing -> s
            Just _  -> foldl' (\ss -> flood ss . (+ xy)) (S.insert xy s) neighbors

solve1, solve2 :: String -> Int
solve1 = sum . fmap ((+1) . digitToInt . snd) . lowPoints . parse
solve2 = product . take 3 . sortBy (flip compare) . fmap S.size . basins . parse
