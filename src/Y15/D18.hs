
module Y15.D18 where

import qualified Data.Vector.Unboxed           as VU
import           Data.Vector.Unboxed.Mutable   (MVector)
import qualified Data.Vector.Unboxed.Mutable   as VUM

import           Util
import           Imports

data YX = YX Int Int

data Board m = Board
    { bounds :: (YX, YX) -- (min,max) coords (inclusive)
    , vector :: MVector (PrimState m) Bool
    }

type TickFn = (YX,YX) -> YX -> Bool -> Int -> Bool

lights :: Parser [[Bool]]
lights =
    rowOfLights `endBy` eol
  where
    rowOfLights :: Parser [Bool]
    rowOfLights = many light

    light :: Parser Bool
    light = char '#' $> True
        <|> char '.' $> False

mkLights :: PrimMonad m => [[Bool]] -> m (Board m)
mkLights xs =
    let h = length xs - 1
        w = (length . head $ xs) - 1
    in Board (YX 0 0, YX h w) <$> (VU.thaw . VU.fromList $ join xs)

unLights :: PrimMonad m  => Board m -> m [[Bool]]
unLights Board{bounds,vector} = do
    let (_, YX _ x0) = bounds -- NOTE no # of rows check
    chunksOf (x0 + 1) . VU.toList <$> VU.freeze vector

getOr :: PrimMonad m => Bool -> Board m -> YX -> m Bool
getOr orV Board{bounds, vector} (YX y x) =
    let (YX y0 x0, YX y1 x1) = bounds
    in if  y < y0 || y > y1
        || x < x0 || x > x1
        then return orV
        else VUM.read vector (y * (x1 + 1) + x)

neighborsOnAround :: PrimMonad m => Board m -> YX -> m Int
neighborsOnAround b (YX y x) =
    length . filter id <$> mapM (getOr False b)
        [ YX (y-1) (x-1), YX (y-1) x, YX (y-1) (x+1)
        , YX  y    (x-1),             YX  y    (x+1)
        , YX (y+1) (x-1), YX (y+1) x, YX (y+1) (x+1)
        ]

tickLights :: PrimMonad m => TickFn -> Board m -> Board m -> m ()
tickLights f src@Board{bounds} dst = do
    let (YX y0 x0, YX y1 x1) = bounds
    forM_ [ (YX y x, y * (x1 + 1) + x)
                | y <- [y0..y1]
                , x <- [x0..x1]] $ \(yx, i) -> do
        v <- VUM.read (vector src) i

        n <- neighborsOnAround src yx
        VUM.write (vector dst) i (f bounds yx v n)

tickTimes :: PrimMonad m => TickFn -> Board m -> Int -> m ()
tickTimes tick b n = do
    dstVector <- VUM.replicate (VUM.length (vector b)) False
    let bb = b {vector = dstVector}
    stepTimes_ n b bb

    when (odd n) $
        VUM.copy (vector b) (vector bb)
  where
    stepTimes_ :: PrimMonad m => Int -> Board m -> Board m -> m ()
    stepTimes_ 0 _ _     = return ()
    stepTimes_ i src dst = do
        tickLights tick src dst
        stepTimes_ (i-1) dst src

solve :: TickFn -> String -> IO Int
solve tick s = do
    b <- mkLights (parseOrDie lights s)
    tickTimes tick b 100
    VUM.foldl' (\a x -> a + bool 0 1 x) 0  (vector b)

-- A light which is on  stays on when 2 or 3 neighbors are on,  and turns off otherwise.
-- A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
tick1 :: TickFn
tick1 _ _ v n =
    (v && (n == 2 || n == 3))
        || (not v &&  n == 3)

-- four lights, one in each corner, are stuck on and can't be turned off.
tick2 :: TickFn
tick2 bounds@(YX ya xa, YX yz xz) yx@(YX y x) v n =
    ((y == ya || y == yz) && (x == xa || x == xz))
        || tick1 bounds  yx v n

solve1, solve2 :: String -> IO Int
solve1 = solve tick1
solve2 = solve tick2
