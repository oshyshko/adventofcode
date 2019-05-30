module Y15.D06ST where

import           Control.Monad      (mapM_)
import           Control.Monad.ST   (ST)
import           Data.Array.ST      (STUArray, newArray, runSTUArray)
import           Data.Array.Unboxed (UArray, assocs)

import           Y15.D06Shared

sumApplyCommands :: (Op -> Int -> Int) -> [Command] -> Int
sumApplyCommands f xs =
    let a :: UArray Int Int
        a = runSTUArray $ do
            m <- newArray (0, side * side -1) 0 :: ST s (STUArray s Int Int)
            mapM_ (applyCommand f m) xs
            return m
    in  sum [v | (_, v) <- assocs a]

solve1 :: String -> Int
solve1 s = either
    (error . show)
    (sumApplyCommands apply1)
    (parseCommands s)

solve2 :: String -> Int
solve2 s = either
    (error . show)
    (sumApplyCommands apply2)
    (parseCommands s)
