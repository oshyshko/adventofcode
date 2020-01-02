module Y15.D18 where

import           Control.Monad                 (join, when)
import           Data.Array.IArray             (elems)
import           Data.Array.IO                 (IOUArray)
import           Data.Array.MArray             (MArray, getAssocs, getBounds,
                                                getElems, newArray,
                                                newListArray, readArray,
                                                writeArray)
import           Data.Array.ST                 (runSTUArray)
import           Data.Array.Unboxed            (UArray)
import           Data.Functor                  (($>))
import           Data.List.Split               (chunksOf)
import           Text.ParserCombinators.Parsec (Parser, char, endBy, many,
                                                (<|>))

import           Util

type YX = (Int, Int)

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
    in newListArray ((0,0), (h-1,w-1)) $ join xs

unLights :: MArray a Bool m => a YX Bool -> m [[Bool]]
unLights m = do
    ((_, xa), (_, xz)) <- getBounds m -- NOTE no # of rows check
    chunksOf (xz-xa+1) <$> getElems m

getOr :: MArray a e m => e -> a YX e -> (YX, YX) -> YX -> m e
getOr orV m ((ax,ay),(zx,zy)) yx@(y,x) =
    if     x < ax || y < ay
        || x > zx || y > zy
        then return orV
        else readArray m yx

-- neighborsOnAround :: IOUArray YX Bool -> (YX, YX) -> YX -> IO Int
neighborsOnAround :: MArray a Bool m => a YX Bool -> (YX, YX) -> YX -> m Int
neighborsOnAround m yxaz (y,x) =
    length . filter id <$> mapM (getOr False m yxaz)
        [ (y-1, x-1), (y-1, x), (y-1, x+1)
        , (y  , x-1),           (y  , x+1)
        , (y+1, x-1), (y+1, x), (y+1, x+1)
        ]

-- A light which is on  stays on when 2 or 3 neighbors are on,  and turns off otherwise.
-- A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
tick1 :: TickFn
tick1 _ _ v n =
    (v && (n == 2 || n == 3))
    || (not v &&  n == 3)

-- four lights, one in each corner, are stuck on and can't be turned off.
tick2 :: TickFn
tick2 yxaz@((y0,x0), (yz,xz)) yx@(y,x) v n =
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

solveIO :: TickFn -> String -> IO Int
solveIO tick s = do
    m <- mkLights (parseOrDie lights s) :: IO (IOUArray YX Bool)
    tickTimes tick m 100
    length . filter id <$> getElems m

solveST :: TickFn -> String -> Int
solveST tick s =
    let res :: UArray YX Bool
        res = runSTUArray $ do
            m <- mkLights (parseOrDie lights s)
            tickTimes tick m 100
            return m
    in length . filter id . elems $ res

solve1IO, solve2IO :: String -> IO Int
solve1IO = solveIO tick1
solve2IO = solveIO tick2

solve1ST, solve2ST :: String -> Int
solve1ST = solveST tick1
solve2ST = solveST tick2