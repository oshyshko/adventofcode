module Util where

import           Control.DeepSeq       (NFData, force)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Debug.Trace           as Trace
import           GHC.IO                (unsafePerformIO)
import           Numeric               (showFFloat)

import           Imports

-- Examples:
-- s <- readInput "Y15.D01"
-- Y15.D14.solve1 <$> readInput "Y15.D14"
readInput :: String -> IO String
readInput name = readFile $ "res/" ++ replaceAll "." "/" (take 7 name) ++ ".txt"

-- lists
replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll from to = intercalate to . splitOn from

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

-- flipped fixes -- number means "number of args" passed to "loop"
fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 = flip fix

fix2 :: a -> b -> ((a -> b -> c) -> a -> b -> c) -> c
fix2 a b loop = fix loop a b

fix3 :: a -> b -> c -> ((a -> b -> c -> d) -> a -> b -> c -> d) -> d
fix3 a b c loop = fix loop a b c

tuplify2 :: [[a]] -> [(a,a)]
tuplify2 = fmap \case
    [a,b] -> (a,b)
    _     -> shouldNeverReachHere

-- > divvy2 1 [1..5]
-- [(1,2), (2,3), (3,4), (4,5)]
divvy2 :: Int -> [a] -> [(a,a)]
divvy2 n = tuplify2 . divvy 2 n

-- > tuples2 [1..4]
-- [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]
tuples2 :: [a] -> [(a,a)]
tuples2 = tuplify2 . tuples 2

insert :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
insert ab       []            = [ab]
insert ab@(a,b) (x@(xa,_):xs) =
    if xa == a
        then (a,b) : xs
        else x : insert ab xs

-- trace
tr :: String -> a -> a
tr = Trace.trace

tr_ :: String -> a -> a
tr_ _ = id

trl :: Show a => String -> a -> a
trl s x = Trace.trace (s ++ "=" ++ show x) x

trl_ :: String -> a -> a
trl_ _ = id

traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

traceShowId :: Show a => a -> a
traceShowId = Trace.traceShowId

traceTime :: NFData a => String -> a -> a
traceTime s f = unsafePerformIO $ do
    (a, millis) <- timeOf (pure f)
    pure $ tr (s <> show millis) a

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
    fix1 (fromIntegral i::Float, "BKMGTPEZY") $ \l -> \case
        (n, u:units) ->
            if n <= 999.9 || null units
                then showFFloat (Just $ if u == 'B' then 0 else 1) n [u]
                else l (n / 1024, units)
        _ -> shouldNeverReachHere

shouldNeverReachHere :: a
shouldNeverReachHere = error "should never reach here"
