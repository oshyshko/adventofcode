module Y15.D18 where

import           Control.Monad                 (join, when)
import           Control.Monad.ST              (ST, runST)
import           Data.Array                    (Ix)
import           Data.Array.MArray             (MArray, getAssocs, getBounds,
                                                getElems, newArray,
                                                newListArray, readArray,
                                                writeArray)
import           Data.Array.ST                 (STUArray)
import           Data.Functor                  (($>))
import           Data.List.Split               (chunksOf)
import           Text.ParserCombinators.Parsec (Parser, char, endBy, many,
                                                (<|>))

import           Util

data YX = YX Int Int
    deriving (Eq, Ord, Show, Read, Bounded, Ix)

-- :: (bounds) -> YX -> alive? -> neighbors-alive -> alive?
type TickFn = (YX, YX) -> YX -> Bool -> Int -> Bool

lights :: Parser [[Bool]]
lights =
    rowOfLights `endBy` eol
  where
    rowOfLights :: Parser [Bool]
    rowOfLights = many light

    light :: Parser Bool
    light = char '#' $> True
        <|> char '.' $> False

mkLights :: MArray a Bool m => [[Bool]] -> m (a YX Bool)
mkLights xs =
    let h = length xs
        w = length . head $ xs
    in newListArray (YX 0 0, YX (h-1) (w-1)) $ join xs

unLights :: MArray a Bool m => a YX Bool -> m [[Bool]]
unLights m = do
    (YX _ xa, YX _ xz) <- getBounds m -- NOTE no # of rows check
    chunksOf (xz-xa+1) <$> getElems m

getOr :: MArray a e m => e -> a YX e -> (YX, YX) -> YX -> m e
getOr orV m (YX ay ax, YX zx zy) yx@(YX y x) =
    if     y < ay || y > zy
        || x < ax || x > zx
        then return orV
        else readArray m yx

-- neighborsOnAround :: IOUArray YX Bool -> (YX, YX) -> YX -> IO Int
neighborsOnAround :: MArray a Bool m => a YX Bool -> (YX, YX) -> YX -> m Int
neighborsOnAround m yxaz (YX y x) =
    length . filter id <$> mapM (getOr False m yxaz)
        [ YX (y-1) (x-1), YX (y-1) x, YX (y-1) (x+1)
        , YX  y    (x-1),             YX  y    (x+1)
        , YX (y+1) (x-1), YX (y+1) x, YX (y+1) (x+1)
        ]

-- A light which is on  stays on when 2 or 3 neighbors are on,  and turns off otherwise.
-- A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
tick1 :: TickFn
tick1 _ _ v n =
    (v && (n == 2 || n == 3))
        || (not v &&  n == 3)

-- four lights, one in each corner, are stuck on and can't be turned off.
tick2 :: TickFn
tick2 yxaz@(YX y0 x0, YX yz xz) yx@(YX y x) v n =
    ((y == y0 || y == yz) && (x == x0 || x == xz))
        || tick1 yxaz yx v n

tickLights :: MArray a Bool m => TickFn -> (YX, YX) -> a YX Bool -> a YX Bool -> m ()
tickLights f yxaz src dst =
    getAssocs src >>= mapM_
        (\(yx, v::Bool) -> do
            n <- neighborsOnAround src yxaz yx
            writeArray dst yx (f yxaz yx v n))

tickTimes :: MArray a Bool m => TickFn -> a YX Bool -> Int -> m ()
tickTimes tick a n = do
    yxaz <- getBounds a
    a' <- newArray yxaz False
    dst <- stepTimes_ n yxaz a a'
    when (odd n) $
        getAssocs dst >>= mapM_ (\(i, e::Bool) -> writeArray a i e)
  where
    stepTimes_ :: (MArray a Bool m) => Int -> (YX, YX) -> a YX Bool -> a YX Bool -> m (a YX Bool)
    stepTimes_ 0 _ src _ = return src
    stepTimes_ i yxaz src dst = do
        tickLights tick yxaz src dst
        stepTimes_ (i-1) yxaz dst src

solve :: TickFn -> String -> Int
solve tick s = runST $ do
    a <- mkLights (parseOrDie lights s) :: ST s (STUArray s YX Bool)
    tickTimes tick a 100
    length . filter id <$> getElems a

solve1, solve2 :: String -> Int
solve1 = solve tick1
solve2 = solve tick2
