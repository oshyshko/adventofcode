module Y21.D19 where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Imports
import           Util

type RegionId = Int
type Coord    = Int
data XYZ      = XYZ Coord Coord Coord deriving (Eq, Ord, Show)

instance Num XYZ where
    (XYZ x y z) + (XYZ a b c) = XYZ (x + a) (y + b) (z + c)
    (XYZ x y z) - (XYZ a b c) = XYZ (x - a) (y - b) (z - c)
    (XYZ x y z) * (XYZ a b c) = XYZ (x * a) (y * b) (z * c)
    abs           (XYZ x y z) = XYZ (abs x) (abs y) (abs z)
    negate        (XYZ x y z) = XYZ (negate x) (negate y) (negate z)
    signum        _           = error "Not implemented"
    fromInteger x             = let v = fromIntegral x in XYZ v v v

data Region = Region
    { regionId   :: RegionId
    , scannerXyz :: XYZ
    , beacons    :: [XYZ]
    } deriving (Show, Eq)

regions :: Parser [Region]
regions =
    region `sepBy` eol
  where
    region = Region
                <$> (string "--- scanner " *> natural <* string " ---" <* eol)
                <*> pure 0
                <*> beacon `endBy` eol
    beacon = XYZ <$> integer <* char ',' <*> integer <* char ',' <*> integer

directions :: [XYZ -> XYZ]
directions =
    [ id
    , \(XYZ x y z) -> XYZ         x  (negate z)         y
    , \(XYZ x y z) -> XYZ         x          z  (negate y)
    , \(XYZ x y z) -> XYZ         y  (negate x)         z
    , \(XYZ x y z) -> XYZ (negate y)         x          z
    , \(XYZ x y z) -> XYZ         x  (negate y) (negate z)
    ]

rolls :: [XYZ -> XYZ]
rolls =
    [ id
    , \(XYZ x y z) -> XYZ (negate z)         y          x
    , \(XYZ x y z) -> XYZ (negate x)         y  (negate z)
    , \(XYZ x y z) -> XYZ         z          y  (negate x)
    ]

diffImprint :: [XYZ] -> [XYZ]
diffImprint beacons =
    [ a - b
    | a <- beacons
    , b <- beacons
    , a /= b
    ]

productImprint :: [XYZ] -> [Coord]
productImprint beacons =
    [ a - b & \(XYZ x y z) -> abs $ x * y * z
    | a <- beacons
    , b <- beacons
    , a /= b
    ]

intersectionCount :: Ord a => Set a -> [a] -> Int
intersectionCount a b =
    S.size a - S.size (foldl' (flip S.delete) a b)

-- > uniquePairs [1..4]
-- [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]
uniquePairs :: [a] -> [(a,a)]
uniquePairs = \case
    []     -> []
    (x:xs) -> ((x,) <$> xs) ++ uniquePairs xs

findLinks :: [Region] ->  Map RegionId [RegionId]
findLinks rs = rs
    & fmap (\r -> (r, productImprint (beacons r)))
    & uniquePairs
    & concatMap (\((arid, api), (brid, bpi)) ->
        let c = intersectionCount (S.fromList api) bpi
        in  [ (c, regionId arid, regionId brid)
            , (c, regionId brid, regionId arid)
            ] )
    & filter (\(c,_,_) -> c >= 3 * 12)  -- keep regions with with 12+ overlapping beacons (3 coords in XZY * 12)
    & fmap   (\(_,a,b) -> (a,[b]))
    & M.fromListWith (<>)

findMergeOrder :: RegionId -> Map RegionId [RegionId] -> [(RegionId, RegionId)]
findMergeOrder start rid2rids =
    go (S.singleton start) [start]
  where
    go :: Set RegionId -> [RegionId] -> [(RegionId, RegionId)]
    go visited q = case q of
        [] -> if visited == S.fromList (M.keys rid2rids)
                then []
                else error $ "Couldn't reach regions: " <> show (M.keys rid2rids)
        (rid:qRem) ->
            let revealed = rid2rids M.! rid
                nq       = filter (`S.notMember` visited) revealed
            in ((rid,) <$> nq) <>
                    go (foldl' (flip S.insert) visited revealed) (qRem <> nq)

findRegionFix :: [XYZ] -> [XYZ] -> (XYZ -> XYZ)
findRegionFix as bs =
    let sa     = S.fromList $ diffImprint as
        orient =
            [ (intersectionCount sa (diffImprint $ f <$> bs), f)
            | d <- directions
            , r <- rolls
            , let f = d . r
            ]
            & maximumBy (compare `on` fst)
            & snd
    in  -- finally, find the common offset and add it to `orient`
          [ a - orient b | a <- as, b <- bs ]
        & fmap (,1::Int)
        & M.fromListWith (+)
        & M.toList
        & maximumBy (compare `on` snd)
        & fst
        & (\offset -> (+ offset) . orient)

merge :: Map RegionId Region -> Map RegionId Region -> (RegionId, RegionId) -> Map RegionId Region
merge rid2r m (aid,bid) =
    let a    = m     M.! aid
        b    = rid2r M.! bid
        f    = findRegionFix (beacons a) (beacons b)
        bNew = Region{ regionId = bid, scannerXyz = f 0, beacons = f <$> beacons b }
    in M.insert bid bNew m

solve :: [Region] -> Map RegionId Region
solve rs =
    let rid2r = M.fromList . fmap (\r -> (regionId r, r)) $ rs
    in findLinks rs                     -- M.fromList [(0,[29,20,17]), (1,[31,8,3]), (2,[4]), ...]
        & findMergeOrder (0::RegionId)  -- [(0,29),(0,20),(0,17),(20,24),(24,23),(24,10), ...]
        & foldl' (merge rid2r) (M.singleton (0::RegionId) (rid2r M.! 0))

solve1, solve2 :: String -> Int
solve1 = S.size . S.fromList . concatMap beacons              . solve . parseOrDie regions
solve2 = maximum . distances . fmap      scannerXyz . M.elems . solve . parseOrDie regions
  where
    distances ss = [ manhattan a b | a <- ss, b <- ss ]
    manhattan a b =
        let XYZ x y z = abs (a - b)
        in x + y + z
