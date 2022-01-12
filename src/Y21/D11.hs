module Y21.D11 where

import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports

type XY = (Int, Int)
data Board m = Board XY (VUM.MVector (PrimState m) Word8) -- wh values

-- 11111
-- 19991
-- 19191
-- 19991
-- 11111
--
parse :: PrimMonad m => String -> m (Board m)
parse s =
    let xs = lines s
        wh = (length . head $ xs, length xs)
        v  = V.thaw . V.fromList . fmap (fromIntegral . digitToInt) . concat $ xs
    in Board wh <$> v

tick :: forall m. PrimMonad m => Board m -> m Int
tick (Board (w,h) v) = do
    forM_ [0..VM.length v-1] $ \i ->        -- inc + cascade
        inc (rem i w, quot i w)
    VM.ifoldM countAndReset 0 v             -- count flashes and reset
  where
    countAndReset a i x
        | x == octoFlashing = VM.write v i octoReset >> pure (a + 1)
        | otherwise         = pure a

    inc :: XY -> m ()
    inc xy@(x, y) = do
        unless (x < 0 || y < 0 || x >= w || y >= h) do
            a <- readV xy
            if | a == octoFlashing  -> pure ()
               | a < octoMax        -> writeV xy (a+1)
               | otherwise          -> do
                   writeV xy octoFlashing
                   forM_ neighbors (inc . add xy)

    octoFlashing      = 255
    octoReset         = 0
    octoMax           = 9

    readV  (x,y)      = VM.read  v (y * w + x)
    writeV (x,y)      = VM.write v (y * w + x)
    add (x, y) (p, q) = (x + p, y + q)
    neighbors         = [ (x,y) | x <- [-1..1], y <- [-1..1], x /=0 || y /= 0 ]

solve1 :: String -> IO Int
solve1 s =
    parse s >>= (replicateM 100 . tick) <&> sum

solve2 :: String -> IO Int
solve2 s = do
    b@(Board (w, h) _) <- parse s
    flip fix 1 $ \loop i -> do
        n <- tick b
        if n == w * h
            then pure i
            else loop (i+1)

-- debug helpers
showBoard :: PrimMonad m => Board m -> m String
showBoard (Board (w,_) v) =
    intercalate "\n" . chunksOf w . fmap (intToDigit . fromIntegral) . V.toList
        <$> V.freeze v
