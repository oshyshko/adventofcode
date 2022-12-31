module Y21.D05 where

import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           Parser
import qualified Point
import           XY

-- 0,9 -> 5,9
-- 8,0 -> 0,8
lineParser :: Parser [(XY,XY)]
lineParser =
    line `endBy` eol
  where
    line = (,) <$> xy <* string " -> " <*> xy
    xy   = XY <$> natural <* char ',' <*> natural

{-# INLINE solve #-}
solve :: forall v m. (PrimMonad m, v ~ Word16) => [(XY,XY)] -> m Int
solve ls = do
    let side = 1000

    s <- VUM.replicate (side * side) 0                      -- create
    forM_ ls (addLine side s)                               -- iterate + alter
    VUM.foldl' (\a x -> a + if x >= 2 then 1 else 0) 0 s    -- fold
  where
    addLine side s (xy0,xy1) =
        let diff = xy1 - xy0
            step = signum diff
            len  = 1 + Point.fold max (abs diff)

            go :: Int -> XY -> m ()
            go 0 _ = return ()
            go l xy@(XY x y) = do
                VM.modify s ((+1) :: v -> v)  (x + y * side)
                go (l - 1) (xy + step)

        in  go len xy0

filterHoriVerti :: [(XY,XY)] -> [(XY,XY)]
filterHoriVerti = filter (\(XY x y, XY a b) -> x == a || y == b)

solve1, solve2 :: String -> IO Int
solve1 = solve . filterHoriVerti . parseOrDie lineParser
solve2 = solve                   . parseOrDie lineParser
