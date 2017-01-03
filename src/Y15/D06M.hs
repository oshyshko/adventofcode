module Y15.D06M where

import           Y15.D06                       (Command, Op (..), XY, apply1,
                                                apply2, commands)

import           Data.Array.IO                 (IOUArray, getElems, newArray,
                                                readArray, writeArray)

import           Text.ParserCombinators.Parsec (parse)


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
sumApplyCommands f xs = do
  m <- newArray (0, side * side -1) 0
  mapM_ (applyCommand f m) xs
  sum . map fromIntegral <$> getElems m

solve1 :: String -> IO Int
solve1 s = either (error . show)
                  (sumApplyCommands apply1)
                  (parse commands "commands" s)

solve2 :: String -> IO Int
solve2 s = either (error . show)
                  (sumApplyCommands apply2)
                  (parse commands "commands" s)
