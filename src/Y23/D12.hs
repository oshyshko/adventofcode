{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Y23.D12 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser

type Pattern = String
type Needle  = Int

-- ???.###          1,1,3
-- ?#?#?#?#?#?#?#?  1,3,1,6
patternsAndCounts :: Parser [(Pattern, [Int])]
patternsAndCounts =
    row `endBy` eol
  where
    row = (,)
        <$> (many1 (oneOf ".#?") <* many1 (char ' '))
        <*> (padded natural `sepBy` char ',')

-- see Y23.TestD12
startsWith :: Pattern -> Int -> Bool -> Bool                        -- haystack -> #-count -> terminal?
startsWith _     (-1) False = True
startsWith _       0  True  = True
startsWith []      _  _     = False
startsWith (p:ps)  n  t
    | p == '?'              = startsWith ps (n-1) t
    | p == '#' && n > 0     = startsWith ps (n-1) t
    | p == '.'              = n == 0 && not t
    | otherwise             = False

arrangementCount :: Pattern -> [Int] -> Int
arrangementCount pat is =
    result $ go mkCache (pat, is)
  where
    go :: (k ~ (Pattern, [Int]), r ~ Int) => Cache k r -> k -> Cached k r
    go c    (p,[])   = Cached c (if '#' `notElem` p then 1 else 0)  -- needles=[] --> +1 result
    go c    ([],_)   = Cached c 0                                   -- pattern=[]
    go c0   (p,n:ns) =
        let t = null ns
            c1r1@(Cached c1 r1) = if startsWith p n t
                then memoize c0 (drop (if t then n else n + 1) p, ns) go
                else Cached c0 0
        in if | (head p /= '#') -> memoize c1 (tail p, n:ns) go & \(Cached c2 r2) -> Cached c2 (r1 + r2)
              | otherwise -> c1r1

-- TODO
-- poor man's memoization
data Cache k r = Cache
    { k2r :: Map k r }

data Cached k r = Cached
    { cache  :: Cache k r
    , result :: r }

mkCache :: Cache k r
mkCache = Cache M.empty

memoize :: forall k r. Ord k
    => Cache k r                        -- cache
    -> k                                -- f arg
    -> (Cache k r -> k -> Cached k r)   -- f
    -> Cached k r                       -- cache + result
memoize c@(Cache m) k f =
    case M.lookup k m of
        Nothing -> f c k & \(Cached (Cache m1) r) -> Cached (Cache (M.insert k r m1)) r
        Just r  -> Cached c r

solve1, solve2 :: String -> Int
solve1 = sum . fmap (uncurry arrangementCount)           . parseOrDie patternsAndCounts
solve2 = sum . fmap (uncurry arrangementCount . times 5) . parseOrDie patternsAndCounts
  where
    times n (p,c) = (intercalate "?" $ replicate n p, concat $ replicate n c)
