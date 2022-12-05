module Y21.D11 where

import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           MVec2
import           Util
import           XY

type Board m = MVec2 m Word8

-- 11111
-- 19991
-- 19191
-- 19991
-- 11111
parse :: PrimMonad m => String -> m (Board m)
parse s =
    let xs = lines s
        wh = XY (length . head $ xs) (length xs)
        v  = V.thaw . V.fromList . fmap (fromIntegral . digitToInt) . concat $ xs
    in MVec2 wh <$> v

tick :: forall m. PrimMonad m => Board m -> m Int
tick mv@(MVec2 (XY w h) v) = do
    mapM_ inc [ XY x y | x <- [0..w], y <- [0..h] ] -- inc + cascade
    VUM.ifoldM countAndReset 0 v                    -- count flashes and reset
  where
    countAndReset a i x
        | x == octoFlashing = VUM.write v i octoReset >> pure (a + 1)
        | otherwise         = pure a

    inc :: XY -> m ()
    inc xy = do
        maybeA <- atMaybe mv xy
        case maybeA of
            Nothing -> pure ()
            Just a
                | a == octoFlashing -> pure ()
                | a < octoMax       -> write mv xy (a+1)
                | otherwise         -> do
                    write mv xy octoFlashing
                    forM_ neighbors (inc . (+ xy))

    octoFlashing    = 255
    octoReset       = 0
    octoMax         = 9
    neighbors       = [ XY x y | x <- [-1..1], y <- [-1..1], x /=0 || y /= 0 ]

solve1 :: String -> IO Int
solve1 s =
    parse s >>= (replicateM 100 . tick) <&> sum

solve2 :: String -> IO Int
solve2 s = do
    b@(MVec2 (XY w h) _) <- parse s
    fix1 1 \loop i -> do
        n <- tick b
        if n == w * h
            then pure i
            else loop (i+1)

-- debug helpers
showBoard :: PrimMonad m => Board m -> m String
showBoard (MVec2 (XY w _) v) =
    intercalate "\n" . chunksOf w . fmap (intToDigit . fromIntegral) . V.toList
        <$> V.freeze v
