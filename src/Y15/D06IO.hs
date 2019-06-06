module Y15.D06IO where

import           Data.Array.IO (IOUArray, getElems, newArray)
import           Util
import           Y15.D06Shared

sumApplyCommands :: (Op -> Int -> Int) -> [Command] -> IO Int
sumApplyCommands f xs = do
    m <- newArray (0, side * side - 1) 0 :: IO (IOUArray Int Int)
    mapM_ (applyCommand f m) xs
    sum <$> getElems m

solve1 :: String -> IO Int
solve1 = sumApplyCommands apply1 . parseOrDie commands

solve2 :: String -> IO Int
solve2 = sumApplyCommands apply2 . parseOrDie commands
