module Y23.D12 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser
import           Util            (fix1)

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
    | p == '?' || p == '#' && n > 0 = startsWith ps (n-1) t
    | p == '.'                      = n == 0 && not t
    | otherwise                     = False

arrangementCount :: (Pattern, [Int]) -> Int
arrangementCount psAndCs = runST $ do
    rkv <- newSTRef (M.empty :: Map (Pattern, [Int]) Int)

    let memoize f k = do
            kv <- readSTRef rkv
            case kv M.!? k of
                Just v  -> pure v
                Nothing -> do
                    v <- f k
                    modifySTRef rkv (M.insert k v)
                    pure v

    fix1 psAndCs (\loop -> \case
        (p,[])   -> pure (if '#' `notElem` p then 1 else 0)     -- needles=[] --> +1 result
        ([],_)   -> pure 0                                      -- pattern=[]
        (p,n:ns) -> do
            let t = null ns

            v0 <- if | startsWith p n t -> memoize loop (drop (n + bool 1 0 t) p, ns)
                     | otherwise        -> pure 0

            v1 <- if | head p /= '#'    -> memoize loop (tail p, n:ns)
                     | otherwise        -> pure 0

            pure $ v0 + v1)

solve1, solve2 :: String -> Int
solve1 = sum . fmap  arrangementCount            . parseOrDie patternsAndCounts
solve2 = sum . fmap (arrangementCount . times 5) . parseOrDie patternsAndCounts
  where
    times n (p,c) = (intercalate "?" $ replicate n p, concat $ replicate n c)
