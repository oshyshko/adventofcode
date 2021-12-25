module Y21.D05 where

import qualified Data.IntMap.Strict          as MI
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

-- a mix of both: monadic and pure approaches in one
class (Monad m, k ~ Int, v ~ Word16) => Storage s k v m where
    emptyS :: k -> m s
    alterS :: (v -> v) -> k -> s -> m s
    foldlS :: (a -> v -> a) -> a -> s -> m a

{-# INLINE solve #-}
solve :: forall s k v m . (Storage s k v m) => [Line k] -> m Int
solve ls = do
    let side = 1000 :: k

    s0 <- emptyS (side * side)                         -- create
    s1 <- foldlM (addLine side) s0 ls                  -- iterate + alter
    foldlS (\a x -> a + if x >= 2 then 1 else 0) 0 s1  -- fold
  where
    addLine :: k -> s -> Line k -> m s
    addLine side s (Line xy0 xy1) =
        let diff = liftA2 (-) xy1 xy0         -- x1 - x0, y1 -y0
            step = signum <$> diff            -- signum $ dx, signum $ dy
            len  = 1 + maximum (abs <$> diff) -- 1 + max (abs dx) (abs dy)

            go :: k -> XY k -> s -> m s
            go 0 _ s1 = return s1
            go l xy@(XY x y) s1 = do
                s2 <- alterS ((+1) :: v -> v)  (x + y * side) s1
                go (l - 1) (liftA2 (+) xy step) s2

        in  go len xy0 s

filterHoriVerti :: Eq k => [Line k] -> [Line k]
filterHoriVerti = filter (\(Line xy0 xy1) -> or $ liftA2 (==) xy0 xy1)

solve1, solve2 :: String -> IO Int
solve1MI, solve2MI :: String -> Int

solve1   =               solve @(VUM.MVector (PrimState IO) Word16) . filterHoriVerti . parseOrDie lineParser
solve2   =               solve @(VUM.MVector (PrimState IO) Word16)                   . parseOrDie lineParser
solve1MI = runIdentity . solve @(MI.IntMap Word16)                  . filterHoriVerti . parseOrDie lineParser
solve2MI = runIdentity . solve @(MI.IntMap Word16)                                    . parseOrDie lineParser

-- instances
instance (PrimMonad m, s ~ PrimState m, k ~ Int, v ~ Word16) => Storage (VUM.MVector s v) k v m where
    emptyS k     = VUM.replicate k 0
    alterS f k s = G.modify s f k >> return s
    foldlS f v s = VUM.foldl' f v s

instance (Monad m, v ~ Word16) => Storage (MI.IntMap v) Int v m where
    emptyS _     = return MI.empty
    alterS f k s = return $ MI.alter (Just . maybe 1 f) k s
    foldlS f v s = return $ MI.foldl' f v s
