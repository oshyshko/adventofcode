module Y21.D22 where

import           Imports
import           Parser
import           XYZ

data Octo a
    = Leaf a
    | Split XYZ [Octo a]
    deriving (Eq, Show)

-- on x=10..12,y=10..12,z=-12..-10
-- off x=9..11,y=9..11,z=9..11
rebootSteps :: Parser [(Bool, Line)]
rebootSteps =
    record `endBy` eol
  where
    record :: Parser (Bool, Line)
    record = do
        v  <- try (string "on" $> True) <|> (string "off" $> False)
        ax <- string " x=" *> integer
        bx <- string ".."  *> integer
        ay <- string ",y=" *> integer
        by <- string ".."  *> integer
        az <- string ",z=" *> integer
        bz <- string ".."  *> integer
        -- convert absolute to relative
        pure (v, Line (XYZ ax ay az) (XYZ (bx-ax+1) (by-ay+1) (bz-az+1)))

bounds :: Line
bounds = Line (fromIntegral $ minBound @Int `quot` 4) (fromIntegral $ maxBound @Int `quot` 2)

intersect :: Line -> Line -> Maybe Line
intersect (Line axyz awhd) (Line bxyz bwhd) =
    let xyz             = xyzzip max axyz bxyz
        whd@(XYZ w h d) = xyzzip min (axyz + awhd) (bxyz + bwhd) - xyz
    in if w <= 0 || h <= 0 || d <= 0
        then Nothing
        else Just $ Line xyz whd

split :: Line -> XYZ -> [Line]
split (Line axyz@(XYZ ax ay az) awhd) r@(XYZ rx ry rz) =
    let (XYZ bw bh bd) = r - axyz
        (XYZ pw ph pd) = axyz + awhd - r
    in  [Line (XYZ x y z) (XYZ w h d)
        | (x,w) <- [(ax,bw), (rx,pw)]
        , (y,h) <- [(ay,bh), (ry,ph)]
        , (z,d) <- [(az,bd), (rz,pd)]
        ]

-- +---------+
-- |         |
-- |  +-.-+  |    <- outerCenter
-- |  |   |  |
-- |  I---+  |
-- O---------+
bestSplitCenter :: Line -> Line -> XYZ
bestSplitCenter (Line axyz@(XYZ ax ay az) awhd) (Line bxyz@(XYZ bx by bz) bwhd) =
    let axyz'@(XYZ ax' ay' az') = axyz + awhd
        (XYZ bx' by' bz')       = bxyz + bwhd
        outerCenter             = axyz + axyz' & xyzmap (`quot` 2)
    in  [ XYZ x y z
        | x <- [bx, bx']
        , y <- [by, by']
        , z <- [bz, bz']
        ]
        & filter (\(XYZ x y z) ->
                (x /= ax && x /= ax')
            || (y /= ay && y /= ay')
            || (z /= az && z /= az') )
        & fmap (\x -> (xyzfold (+) (abs $ outerCenter - x), x))     -- the most close to outer center
        & sortBy (compare `on` fst)
        & minimumBy (compare `on` fst)
        & snd

--      /+---++---Z  axyz + awhd
--    /+ |   ||   |
--   + |/+---+----+
--   |/|/+---++---+
--   |/| |   ||   |
--   | |/+---++---+
--   |/+----//--+
--   A----//---/
-- axyz
--
set :: forall a . Eq a => Octo a -> Line -> Octo a -> Octo a -- new-value, new-box, source
set Split{} _ _ = error "Expected a Leaf, but got Split"
set v@(Leaf newValue) box source =
    go bounds box source
  where
    go :: Line -> Line -> Octo a -> Octo a
    go outer inner o = case o of
        Leaf oldValue ->
            if outer == inner || oldValue == newValue
                then v
                else go outer inner $ Split (bestSplitCenter outer inner) (replicate 8 o)

        Split r os -> Split r (zipWith (updateOcto inner) os $ split outer r)

    updateOcto :: Line -> Octo a -> Line -> Octo a
    updateOcto inner o07 cuboid07 =
        maybe o07                                   -- by default keep the old value
            (\overlap -> go cuboid07 overlap o07)   -- ... otherwise go deeper
            (cuboid07 `intersect` inner)            -- ... when a cuboid overlaps with target

list :: forall a. Octo a -> [(a, Line)]
list =
    go bounds
  where
    go :: Line -> Octo a -> [(a, Line)]
    go outer = \case
        Leaf v     -> [(v, outer)]
        Split r os -> concat $ zipWith go (split outer r) os

volume :: Octo Bool -> Int
volume = sum . concatMap (\(v,Line _ whd) -> [xyzfold (*) whd | v]) . list

solve1, solve2 :: String -> Int
solve1 s =
      parseOrDie rebootSteps s
    & concatMap (\(v,line) -> maybe []
        (\clipped -> [(v,clipped)])
        (line `intersect` Line (-50) 101))
    & foldl' (\o (v, line) -> set (Leaf v) line o) (Leaf False)
    & volume

solve2 s =
      parseOrDie rebootSteps s
    & foldl' (\o (v, line) -> set (Leaf v) line o) (Leaf False)
    & volume
