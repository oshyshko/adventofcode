module Y15.D06IO where

import           Data.Array.IO (IOUArray, getElems, newArray)
import           Y15.D06Shared

sumApplyCommands :: (Op -> Int -> Int) -> [Command] -> IO Int
sumApplyCommands f xs = do
    m <- newArray (0, side * side -1) 0 :: IO (IOUArray Int Int)
    mapM_ (applyCommand f m) xs
    sum <$> getElems m

solve1 :: String -> IO Int
solve1 s = either
    (error . show)
    (sumApplyCommands apply1)
    (parseCommands s)

solve2 :: String -> IO Int
solve2 s = either
    (error . show)
    (sumApplyCommands apply2)
    (parseCommands s)
