module Y23.D16 where

import qualified Data.Bits as B

import           Geom.XY
import           Imports
import           MVec2     (MVec2 (..))
import qualified MVec2     as MV
import           Util
import qualified Vec2      as V
import           Vec2      (Vec2 (..))

type Cell = Char
type Grid = Vec2 Cell
type Dir  = XY
type Dirs = Word8

type GridDirs m = MVec2 m Dirs

-- .|...\....
-- |.-.\.....
-- .....|-...
-- ......../.
parse :: String -> Grid
parse = V.fromList . lines

fillAndCount :: XY -> Dir -> Grid -> Int
fillAndCount sxy sd v@Vec2{wh} =
    runST $ do
        visited <- MV.replicate wh sEmpty
        fix1 [(sxy,sd)] \loop -> \case
            [] -> pure ()
            ((xy,d):open) ->
                (v V.!? xy) & \case
                    Nothing -> loop open                                -- skip out of bounds
                    Just c -> do
                        dirs <- MV.read visited xy
                        if d `sMember` dirs
                            then loop open                              -- skip seen dir
                            else do                                     -- visit new dir
                                MV.write visited xy (sInsert d dirs)
                                loop (nextSteps xy d c ++ open)

        MV.foldl' (\a x -> a + if x == 0 then 0 else 1) 0 visited
  where
    nextSteps :: XY -> Dir -> Cell -> [(XY, Dir)]
    nextSteps xy d = \case
        '.'  ->                          [(xy+d,d)]
        '-'  -> if d == L || d == R then [(xy+d,d)] else [(xy,L), (xy,R)]
        '|'  -> if d == U || d == D then [(xy+d,d)] else [(xy,U), (xy,D)]
        '\\' -> (case d of U->L; L->U; R->D; D->R; _ -> shouldNeverReachHere) & \nd -> [(xy+nd,nd)]
        '/'  -> (case d of U->R; R->U; L->D; D->L; _ -> shouldNeverReachHere) & \nd -> [(xy+nd,nd)]
        _    -> shouldNeverReachHere

    -- set replacements
    sEmpty          = B.zeroBits @Word8
    sInsert d dirs  = B.setBit dirs (dir2index d)
    sMember d dirs  = B.testBit dirs (dir2index d)
    dir2index       = \case U->0; D->1; L->2; R->4; _->shouldNeverReachHere

solve1 :: String -> Int
solve1 = fillAndCount 0 R . parse

solve2 :: String -> Int
solve2 s =
    let v@Vec2{wh} = parse s
        (w1,h1) = (getX wh - 1, getY wh - 1)
    in
             [(XY  x  0, D) | x <- [0..w1]]
          ++ [(XY  x h1, U) | x <- [0..w1]]
          ++ [(XY  0  y, R) | y <- [0..h1]]
          ++ [(XY w1  y, L) | y <- [0..h1]]
        & fmap (\(xy,d) -> fillAndCount xy d v)
        & maximum
