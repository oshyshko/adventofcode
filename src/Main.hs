module Main where

import qualified Y15.D01
import qualified Y15.D02
import qualified Y15.D03
import qualified Y15.D04
import qualified Y15.D05
import qualified Y15.D06
import qualified Y15.D06M
import qualified Y15.D07
import qualified Y15.D08
import qualified Y15.D09
import qualified Y15.D10

import           Control.DeepSeq       (NFData, force)
import           Control.Exception     (evaluate)
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Data.Ratio            (numerator)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to = intercalate to . splitOn from

main :: IO ()
main = mapM_
    (\ (name, solvers) -> do
        input <- readFile $ "res/" ++ replace "." "/" (take 7 name) ++ ".txt"
        printf "%-8s --> " name
        hFlush stdout
        at <- mapM (\solve -> timeOf (solve input)) solvers
        printf "%-20s solved within %s ms\n"
            (show . map fst $ at)
            (show . map snd $ at))
    days

days :: [(String, [String -> IO Int])]
days = [ ("Y15.D01", mr [Y15.D01.solve1,  Y15.D01.solve2])
       , ("Y15.D02", mr [Y15.D02.solve1,  Y15.D02.solve2])
       , ("Y15.D03", mr [Y15.D03.solve1,  Y15.D03.solve2])
       , ("Y15.D04", mr [Y15.D04.solve1,  Y15.D04.solve2])
       , ("Y15.D05", mr [Y15.D05.solve1,  Y15.D05.solve2])
       , ("Y15.D06", mr [Y15.D06.solve1,  Y15.D06.solve2])
       , ("Y15.D06M",   [Y15.D06M.solve1, Y15.D06M.solve2])
       , ("Y15.D07", mr [Y15.D07.solve1,  Y15.D07.solve2])
       , ("Y15.D08", mr [Y15.D08.solve1,  Y15.D08.solve2])
       , ("Y15.D09", mr [Y15.D09.solve1,  Y15.D09.solve2])
       , ("Y15.D10", mr [Y15.D10.solve1,  Y15.D10.solve2])
       ] where mr = fmap (return .)

timeOf :: NFData a => IO a -> IO (a, Integer)
timeOf ioa = do
    start <- timeInMillis
    a <- force <$> ioa
    evaluate a
    end <- timeInMillis
    pure (a, end - start)
  where
    timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    timeInMillis = (`div` 1000) . fromIntegral <$> timeInMicros
