module Y21.D11 where

import qualified Data.Vector.Unboxed.Mutable as VUM

import           Geom.XY
import           Imports
import           MVec2                       (MVec2 (..))
import qualified MVec2                       as MV
import           Util
import qualified Vec2                        as V

type Energy = Word8

-- 11111
-- 19991
-- 19191
-- 19991
-- 11111
parse :: PrimMonad m => String -> m (MVec2 m Energy)
parse =
      V.thaw
    . V.fromList
    . fmap (fmap $ fromIntegral . digitToInt)
    . lines

tick :: forall m. PrimMonad m => MVec2 m Energy -> m Int
tick mv@(MVec2 (XY w h) v) = do
    mapM_ inc [ XY x y | x <- [0..w], y <- [0..h] ] -- inc + cascade
    VUM.ifoldM countAndReset 0 v                    -- count flashes and reset
  where
    countAndReset a i x
        | x == octoFlashing = VUM.write v i octoReset >> pure (a + 1)
        | otherwise         = pure a

    inc :: XY -> m ()
    inc xy = do
        maybeA <- MV.readMaybe mv xy
        case maybeA of
            Nothing -> pure ()
            Just a
                | a == octoFlashing -> pure ()
                | a < octoMax       -> MV.write mv xy (a+1)
                | otherwise         -> do
                    MV.write mv xy octoFlashing
                    forM_ neighbors (inc . (+ xy))

    octoFlashing    = 255
    octoReset       = 0
    octoMax         = 9
    neighbors       = [ XY x y | x <- [-1..1], y <- [-1..1], x /=0 || y /= 0 ]

solve1 :: String -> IO Int
solve1 s = parse s >>= replicateM 100 . tick <&> sum

solve2 :: String -> IO Int
solve2 s = parse s >>= \v@(MVec2 (XY w h) _) ->
    fix1 1 \loop i -> do
        n <- tick v
        if n == w * h
            then pure i
            else loop (i+1)

-- debug helpers
showBoard :: PrimMonad m => MVec2 m Energy -> m String
showBoard = fmap (show . V.map (intToDigit . fromIntegral)) . V.freeze
