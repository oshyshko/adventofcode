
module Y15.D18 where

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           MVec2 (MVec2(..))
import           Parser
import           XY

type Board m = MVec2 m Bool

-- NOTE: copied from MVec2 to get 532MB -> 486MB allocation savin
-- NOTE: how to make it work without copying?
getOr :: (PrimMonad m, VUM.Unbox v) => v -> MVec2 m v -> XY -> m v
getOr orV (MVec2 wh@(XY w h) vec) xy@(XY x y) =
    if x < 0 || y < 0 || x >= w || y >= h
        then return orV
        else VUM.read vec (xy2i wh xy)

type TickFn = WH -> XY -> Bool -> Int -> Bool

lights :: Parser [[Bool]]
lights =
    rowOfLights `endBy` eol
  where
    rowOfLights :: Parser [Bool]
    rowOfLights = many light

    light :: Parser Bool
    light = char '#' $> True
        <|> char '.' $> False

mkLights :: PrimMonad m => [[Bool]] -> m (MVec2 m Bool)
mkLights xs =
    let h = length xs
        w = length . head $ xs
    in MVec2 (XY w h) <$> (VU.thaw . VU.fromList $ join xs)

unLights :: PrimMonad m  => MVec2 m Bool -> m [[Bool]]
unLights (MVec2 (XY w _) vec) =
    chunksOf w . VU.toList <$> VU.freeze vec

neighborsOnAround :: PrimMonad m => MVec2 m Bool -> XY -> m Int
neighborsOnAround b (XY x y) =
    length . filter id <$> mapM (getOr False b)
        [ XY (x-1) (y-1) , XY  x (y-1), XY (x+1) (y-1)
        , XY (x-1)  y    ,              XY (x+1)  y
        , XY (x-1) (y+1) , XY  x (y+1), XY (x+1) (y+1)
        ]
        -- NOTE: alternative, produces 486MB -> 814MB allocations
        -- [ xy + XY xx yy | xx <- [-1..1], yy <- [-1..1], x /=0 || y /= 0 ]

tickLights :: PrimMonad m => TickFn -> MVec2 m Bool -> Board m -> m ()
tickLights f src@(MVec2 wh@(XY w h) _) dst =
    forM_ [ (XY x y, y * w + x)
          | y <- [0..h-1]
          , x <- [0..w-1]
          ] $ \(xy, i) -> do

        v <- VUM.read (vec src) i
        n <- neighborsOnAround src xy
        VUM.write (vec dst) i (f wh xy v n)

tickTimes :: PrimMonad m => TickFn -> MVec2 m Bool -> Int -> m ()
tickTimes tick b n = do
    dstVec <- VUM.replicate (VUM.length (vec b)) False
    let bb = b {vec = dstVec}
    stepTimes_ n b bb

    when (odd n) $
        VUM.copy (vec b) (vec bb)
  where
    stepTimes_ :: PrimMonad m => Int -> MVec2 m Bool -> MVec2 m Bool -> m ()
    stepTimes_ 0 _ _     = return ()
    stepTimes_ i src dst = do
        tickLights tick src dst
        stepTimes_ (i-1) dst src

solve :: TickFn -> String -> IO Int
solve tick s = do
    b <- mkLights (parseOrDie lights s)
    tickTimes tick b 100
    VUM.foldl' (\a x -> a + bool 0 1 x) 0  (vec b)

-- A light which is on  stays on when 2 or 3 neighbors are on,  and turns off otherwise.
-- A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
tick1 :: TickFn
tick1 _ _ v n =
       (    v && (n == 2 || n == 3))
    || (not v && n == 3)

-- four lights, one in each corner, are stuck on and can't be turned off.
tick2 :: TickFn
tick2 wh@(XY w h) xy@(XY x y) v n =
    (x == 0 || x == w - 1) && (y == 0 || y == h - 1)
        || tick1 wh xy v n

solve1, solve2 :: String -> IO Int
solve1 = solve tick1
solve2 = solve tick2
