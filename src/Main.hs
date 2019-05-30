module Main where

import qualified Y15.D01
import qualified Y15.D02
import qualified Y15.D03
import qualified Y15.D04
import qualified Y15.D05
import qualified Y15.D06HM
import qualified Y15.D06IO
import qualified Y15.D06ST
import qualified Y15.D07
import qualified Y15.D08
import qualified Y15.D09
import qualified Y15.D10
import qualified Y15.D11
import qualified Y15.D12
import qualified Y15.D13

import           Control.DeepSeq       (NFData, force)
import           Control.Exception     (evaluate)
import           Control.Monad         (void)
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Data.Ratio            (numerator)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to = intercalate to . splitOn from

readInput :: String -> IO String
readInput name = readFile $ "res/" ++ replace "." "/" (take 7 name) ++ ".txt"

main :: IO ()
main = mapM_
    (\ (name, solvers) -> do
        input <- readInput name
        printf "%-9s --> " name
        hFlush stdout
        at <- mapM (\solve -> timeOf (solve input)) solvers
        printf "%-18s -- solved within %s ms\n"
            (intercalate ", " $ map fst at)
            (intercalate ", " $ map (show . snd) at))
    days

days :: [(String, [String -> IO String])]
days =
    [ ("Y15.D01",  i2ios   [Y15.D01.solve1,   Y15.D01.solve2])
    , ("Y15.D02",  i2ios   [Y15.D02.solve1,   Y15.D02.solve2])
    , ("Y15.D03",  i2ios   [Y15.D03.solve1,   Y15.D03.solve2])
    , ("Y15.D04",  i2ios   [Y15.D04.solve1,   Y15.D04.solve2])
    , ("Y15.D05",  i2ios   [Y15.D05.solve1,   Y15.D05.solve2])
    , ("Y15.D06HM",i2ios   [Y15.D06HM.solve1, Y15.D06HM.solve2]) -- Data.Map.Strict
    , ("Y15.D06IO",ioi2ios [Y15.D06IO.solve1, Y15.D06IO.solve2]) -- Data.Array.IO
    , ("Y15.D06ST",i2ios   [Y15.D06ST.solve1, Y15.D06ST.solve2]) -- Data.Array.ST
    , ("Y15.D07",  i2ios   [Y15.D07.solve1,   Y15.D07.solve2])
    , ("Y15.D08",  i2ios   [Y15.D08.solve1,   Y15.D08.solve2])
    , ("Y15.D09",  i2ios   [Y15.D09.solve1,   Y15.D09.solve2])
    , ("Y15.D10",  i2ios   [Y15.D10.solve1,   Y15.D10.solve2])
    , ("Y15.D11",  s2ios   [Y15.D11.solve1,   Y15.D11.solve2])
    , ("Y15.D12",  i2ios   [Y15.D12.solve1,   Y15.D12.solve2])
    , ("Y15.D13",  i2ios   [Y15.D13.solve1,   Y15.D13.solve2])
    ]
  where s2ios :: [a -> b] -> [a -> IO b]
        s2ios   = fmap (return .)
        i2ios :: [a -> Int] -> [a -> IO String]
        i2ios   = fmap ((return . show) .)
        ioi2ios :: [a -> IO Int] -> [a -> IO String]
        ioi2ios = fmap (fmap show .)

timeOf :: NFData a => IO a -> IO (a, Integer)
timeOf ioa = do
    start <- timeInMillis
    a <- force <$> ioa
    void $ evaluate a
    end <- timeInMillis
    pure (a, end - start)
  where
    timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    timeInMillis = (`div` 1000) . fromIntegral <$> timeInMicros
