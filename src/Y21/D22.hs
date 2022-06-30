module Y21.D22 where

import           Imports
import           Parser
import           XYZ

data Octo
    = Leaf Bool
    | Split XYZ Octo Octo Octo Octo Octo Octo Octo Octo
    deriving (Eq, Ord, Show)

-- on x=10..12,y=10..12,z=-12..-10
-- off x=9..11,y=9..11,z=9..11
rebootSteps :: Parser [(Bool, Line)]
rebootSteps =
    record `endBy` eol
  where
    record :: Parser (Bool, Line)
    record = do
        onoff <- try (string "on" $> True) <|> (string "off" $> False)
        ax    <- string " x=" *> integer
        bx    <- string ".."  *> integer
        ay    <- string ",y=" *> integer
        by    <- string ".."  *> integer
        az    <- string ",z=" *> integer
        bz    <- string ".."  *> integer
        -- convert absolute to relative
        pure (onoff, mkCuboid (XYZ ax ay az) (XYZ (bx-ax+1) (by-ay+1) (bz-az+1)))

bounds :: Line
bounds =
    mkCuboid (fromIntegral mi) (fromIntegral ma)
  where
    mi = (minBound :: Int) `quot` 4
    ma = (maxBound :: Int) `quot` 2

mkCuboid :: XYZ -> XYZ -> Line
mkCuboid xyz whd@(XYZ w h d) =
    if w < 0 || h < 0 || d < 0
        then error $ "Can't make a cuboid from: " <> show (Line xyz whd)
        else Line xyz whd

intersect :: Line -> Line -> Maybe Line
intersect
        a@(Line axyz awhd@(XYZ aw ah ad))
        b@(Line bxyz bwhd@(XYZ bw bh bd))
    | aw < 0 || ah < 0 || ad < 0 || bw < 0 || bh < 0 || bd < 0 = error $ "Non-positive size: intersect (" <> show a <> ") (" <> show b <> ")"
    | otherwise =
        let xyz             = xyzzip max axyz bxyz
            whd@(XYZ w h d) = xyzzip min (axyz + awhd) (bxyz + bwhd) - xyz
        in if w <= 0 || h <= 0 || d <= 0
            then Nothing
            else Just $ Line xyz whd

inside :: Line -> XYZ -> Bool
inside (Line axyz@(XYZ ax ay az) awhd) (XYZ rx ry rz) =
    let (XYZ ax' ay' az') = axyz + awhd
    in  ax <= rx  && ay <= ry  && az <= rz &&
        rx <= ax' && ry <= ay' && rz <= az'

cuboidInside :: Line -> Line -> Bool
cuboidInside _outer@(Line axyz@(XYZ ax ay az) awhd) _inner@(Line bxyz@(XYZ bx by bz) bwhd) =
    let (XYZ ax' ay' az') = axyz + awhd
        (XYZ bx' by' bz') = bxyz + bwhd
    in  ax  <= bx  && ay  <= by  && az  <= bz  &&
        bx' <= ax' && by' <= ay' && bz' <= az'

getCuboids :: Line -> XYZ -> T8 Line Line Line Line Line Line Line Line
getCuboids a@(Line axyz@(XYZ ax ay az) awhd@(XYZ aw ah ad)) r@(XYZ rx ry rz)
    | not $ inside a r              = error $ "getCuboids: not inside: getCuboids a=" <> show a <> ", r=" <> show r
    | aw < 0 && ah < 0 && ad < 0    = error $ "getCuboids: non-positive size: getCuboids a=" <> show a <> ", r=" <> show r
    | axyz == r                     = error $ "getCuboids: axyz == r, a=" <> show a <> ", r=" <> show r
    | axyz + awhd == r              = error $ "getCuboids: axyz + awhd == r, a=" <> show a <> ", r=" <> show r
    | otherwise =
        let (XYZ bw bh bd) = r - axyz
            (XYZ pw ph pd) = axyz + awhd - r
        in T8   (mkCuboid (XYZ ax ay az) (XYZ bw bh bd))
                (mkCuboid (XYZ ax ay rz) (XYZ bw bh pd))
                (mkCuboid (XYZ ax ry az) (XYZ bw ph bd))
                (mkCuboid (XYZ ax ry rz) (XYZ bw ph pd))
                (mkCuboid (XYZ rx ay az) (XYZ pw bh bd))
                (mkCuboid (XYZ rx ay rz) (XYZ pw bh pd))
                (mkCuboid (XYZ rx ry az) (XYZ pw ph bd))
                (mkCuboid (XYZ rx ry rz) (XYZ pw ph pd))

-- +---------+
-- |         |
-- |  +-.-+  |    <- outerCenter
-- |  |   |  |
-- |  I---+  |
-- O---------+
bestSplitCenter :: Line -> Line -> XYZ
bestSplitCenter outer@(Line axyz@(XYZ ax ay az) awhd) inner@(Line bxyz@(XYZ bx by bz) bwhd)
    | outer == inner                    = error $ "bestSplitCenter: Didn't expect outer == inner: outer=" <> show outer <> ", inner=" <> show inner
    | not $ cuboidInside outer inner    = error $ "bestSplitCenter: Expected cuboid to be inside: outer=" <> show outer <> ", inner=" <> show inner
    | otherwise =
        let axyz'@(XYZ ax' ay' az') = axyz + awhd
            (XYZ bx' by' bz') = bxyz + bwhd
            outerCenter = axyz + axyz' & xyzmap (`quot` 2)
        in  [ XYZ bx  by  bz
            , XYZ bx  by  bz'
            , XYZ bx  by' bz
            , XYZ bx  by' bz'
            , XYZ bx' by  bz
            , XYZ bx' by  bz'
            , XYZ bx' by' bz
            , XYZ bx' by' bz'
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
set :: Octo -> Line -> Octo -> Octo  -- new-value, new-box, source
set Split{} _ _ = error "Expected Leaf, but got Split"
set v@(Leaf sv) box source =
    go bounds box source
  where
    go :: Line -> Line -> Octo -> Octo
    go a@(Line _ (XYZ aw ah ad)) b@(Line _ (XYZ bw bh bd)) o
        | aw <=0 || ah <= 0 || ad <= 0 || bw <=0 || bd <= 0 || bh <= 0 = error $ "Non-positive size: set.go a=" <> show a <> ", b=" <> show b
        | otherwise =
            case o of
                Leaf lv ->
                    if a == b || lv == sv
                        then v
                        else go a b $ Split (bestSplitCenter a b) o o o o o o o o

                Split r o0 o1 o2 o3 o4 o5 o6 o7 ->
                    let (T8 c0 c1 c2 c3 c4 c5 c6 c7) = getCuboids a r
                    in Split r (g o0 c0) (g o1 c1) (g o2 c2) (g o3 c3) (g o4 c4) (g o5 c5) (g o6 c6) (g o7 c7)
                  where
                    g :: Octo -> Line -> Octo
                    g o07 cuboid07 =
                        maybe o07                                   -- by default keep the old value
                            (\overlap -> go cuboid07 overlap o07)   -- ... otherwise go deeper
                            (cuboid07 `intersect` b)                -- ... when a cuboid overlaps with target

list :: Octo -> [(Bool, Line)]
list =
    go bounds
  where
    go :: Line -> Octo -> [(Bool, Line)]
    go outer = \case
        Leaf v -> [(v, outer)]
        Split r o0 o1 o2 o3 o4 o5 o6 o7 ->
            let (T8 c0 c1 c2 c3 c4 c5 c6 c7) =  getCuboids outer r
            in go c0 o0 <> go c1 o1 <> go c2 o2 <> go c3 o3 <> go c4 o4 <> go c5 o5 <> go c6 o6 <> go c7 o7

volume :: Octo -> Int
volume = sum . concatMap (\(v,Line _ whd) -> [xyzfold (*) whd | v]) . list

solve1, solve2 :: String -> Int
solve1 s =
      parseOrDie rebootSteps s
    & concatMap (\(onoff,line) ->
        maybe [] (\x -> [(onoff,x)]) $ intersect line (mkCuboid (-50) 101))
    & foldl' (\oct (v, line) -> set (Leaf v) line oct) (Leaf False)
    & volume

solve2 s =
      parseOrDie rebootSteps s
    & foldl' (\oct (v, line) -> set (Leaf v) line oct) (Leaf False)
    & volume
