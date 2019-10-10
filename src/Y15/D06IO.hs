module Y15.D06IO where

import           Control.Monad (forM_)
import           Data.Array.IO (IOUArray, getElems, newArray)

import           Util
import           Y15.D06Shared

sumApplyCommands :: (Op -> Brightness -> Brightness) -> [Command] -> IO Int
sumApplyCommands f xs = do
    m <- newArray ((0,0), (side-1,side-1)) 0 :: IO (IOUArray XY Brightness)
    forM_ xs $ applyCommandArray f m
    sum <$> getElems m

solve1 :: String -> IO Int
solve1 = sumApplyCommands apply1 . parseOrDie commands

solve2 :: String -> IO Int
solve2 = sumApplyCommands apply2 . parseOrDie commands
