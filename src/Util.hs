module Util where

import           Control.DeepSeq       (NFData, force)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Debug.Trace           as Trace
import           GHC.IO                (unsafePerformIO)
import           Numeric               (showFFloat)

import           Imports

replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll from to = intercalate to . splitOn from

-- Examples:
-- s <- readInput "Y15.D01"
-- Y15.D14.solve1 <$> readInput "Y15.D14"
readInput :: String -> IO String
readInput name = readFile $ "res/" ++ replaceAll "." "/" (take 7 name) ++ ".txt"

-- trace
tr :: Show a => String -> a -> a
tr s x = trace (s <> ": " <> show x) x

trace :: String -> a -> a
trace = Trace.trace

traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

traceShowId :: Show a => a -> a
traceShowId = Trace.traceShowId

traceTime :: NFData a => String -> a -> a
traceTime s f = unsafePerformIO $ do
    (a, millis) <- timeOf (pure f)
    pure $ trace (s <> show millis) a

timeOf :: NFData a => IO a -> IO (a, Integer)
timeOf ioa = do
    start <- timeInMillis
    a <- force <$> ioa
    void $ evaluate a
    end <- timeInMillis
    pure (a, end - start)
  where
    timeInMillis = ceiling . (1000 *) <$> getPOSIXTime

size2humanSize :: Integer -> String
size2humanSize i =
    flip fix (fromIntegral i::Float, "BKMGTPEZY") $ \l (n, u:units) ->
        if n <= 999.9 || null units
            then showFFloat (Just $ if u == 'B' then 0 else 1) n [u]
            else l (n / 1024, units)
