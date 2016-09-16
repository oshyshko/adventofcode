module Day06Perf where

import Day06         (Command, Op (..), XY, parseCommands, solve1, solve2)

import Data.Array.IO (IOUArray, getElems, newArray, readArray, writeArray)

type Matrix = IOUArray Int Int

side :: Int
side = 1000

get :: Matrix -> XY -> IO Int
get m (x,y) = readArray m (x + y * side)

set :: Matrix -> XY -> Int -> IO ()
set m (x,y) = writeArray m (x + y * side)

applyCommand :: (Op -> Int -> Int) -> Matrix -> Command -> IO ()
applyCommand f m (op, (x0,y0), (x1,y1)) =
  mapM_ (\ xy -> do v <- get m xy
                    set m xy (f op v))

        [ (x,y) | y <- [y0..y1],
                  x <- [x0..x1]]

sumApplyCommands :: (Op -> Int -> Int) -> [Command] -> IO Int
sumApplyCommands f commands = do
  m <- newArray (0, side * side -1) 0
  mapM_ (applyCommand f m) commands
  sum . map fromIntegral <$> getElems m

main :: IO ()
main = do
  s <- readFile "Day06.txt"
  case parseCommands s of
    Left e   -> error $ show e
    Right xs -> print =<< (sequence <$> sequence [ sumApplyCommands solve1
                                                 , sumApplyCommands solve2
                                                 ] $ xs)
