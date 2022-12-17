module Y15.D18 where

import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           MVec2                       (MVec2 (..))
import qualified MVec2                       as MV
import           Parser
import qualified Vec2                        as V
import           XY

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

neighborsOnAround :: PrimMonad m => MVec2 m Bool -> XY -> m Int
neighborsOnAround v xy =
    length . filter id <$> mapM (MV.readOr False v . (+ xy))
        [ XY (-1) (-1), XY  0 (-1), XY   1  (-1)
        , XY (-1)   0 ,             XY   1    0
        , XY (-1)   1 , XY  0   1 , XY   1    1
        ]
        -- NOTE: alternative, produces 486MB -> 569MB allocations
        -- [ XY x y | x <- [-1,0,1], y <- [-1,0,1], x /=0 || y /= 0 ]

tickLights :: PrimMonad m => TickFn -> MVec2 m Bool ->  MVec2 m Bool -> m ()
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
    b <- V.thaw . V.fromList . parseOrDie lights $ s
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
