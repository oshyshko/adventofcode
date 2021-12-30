module Y21.D05 where

import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           Util

data XY a   = XY   a a           deriving (Show, Functor, Foldable)
data Line a = Line (XY a) (XY a) deriving Show

instance Applicative XY where
    pure a = XY a a
    liftA2 f (XY x0 y0) (XY x1 y1) = XY (f x0 x1) (f y0 y1)

-- 0,9 -> 5,9
-- 8,0 -> 0,8
lineParser :: Read a => Parser [Line a]
lineParser =
    line `endBy` eol
  where
    line = Line <$> xy <* string " -> " <*> xy
    xy   = XY <$> decimal <* char ',' <*> decimal

{-# INLINE solve #-}
solve :: forall k v m. (PrimMonad m, k ~ Int, v ~ Word16) => [Line k] -> m Int
solve ls = do
    let side = 1000 :: k

    s <- VUM.replicate (side * side) 0                      -- create
    forM_ ls (addLine side s)                               -- iterate + alter
    VUM.foldl' (\a x -> a + if x >= 2 then 1 else 0) 0 s    -- fold
  where
    addLine side s (Line xy0 xy1) =
        let diff = liftA2 (-) xy1 xy0         -- x1 - x0, y1 -y0
            step = signum <$> diff            -- signum $ dx, signum $ dy
            len  = 1 + maximum (abs <$> diff) -- 1 + max (abs dx) (abs dy)

            go :: k -> XY k -> m ()
            go 0 _ = return ()
            go l xy@(XY x y) = do
                G.modify s ((+1) :: v -> v)  (x + y * side)
                go (l - 1) (liftA2 (+) xy step)

        in  go len xy0

filterHoriVerti :: Eq k => [Line k] -> [Line k]
filterHoriVerti = filter (\(Line xy0 xy1) -> or $ liftA2 (==) xy0 xy1)

solve1, solve2 :: String -> IO Int
solve1 = solve . filterHoriVerti . parseOrDie lineParser
solve2 = solve                   . parseOrDie lineParser
