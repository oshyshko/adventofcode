module Y15.D06ST where

import           Control.Monad      (forM_)
import           Control.Monad.ST   (ST)
import           Data.Array.MArray  (newArray)
import           Data.Array.ST      (STUArray, runSTUArray)
import           Data.Array.Unboxed (UArray, elems)

import           Util
import           Y15.D06Shared

sumApplyCommands :: (Brightness -> Op -> Brightness) -> [Command] -> Int
sumApplyCommands f xs =
    let a :: UArray XY Brightness
        a = runSTUArray $ do
            m <- newArray ((0,0), (side-1,side-1)) 0 :: ST s (STUArray s XY Brightness)
            forM_ xs (applyCommandArray f m)
            return m
    in  sum . elems $ a

solve1 :: String -> Int
solve1 = sumApplyCommands apply1 . parseOrDie commands

solve2 :: String -> Int
solve2 = sumApplyCommands apply2 . parseOrDie commands
