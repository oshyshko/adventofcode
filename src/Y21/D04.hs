module Y21.D04 where
-- NOTE: an impure solution using mutable vectors (ugly, but fun)

import           Data.Bits                   ((.|.))
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Unboxed         as VU

import           Imports
import           Util

type PrimV m s v a = (PrimMonad m, s ~ PrimState m, G.MVector v a, a ~ Number)

type Number = Word8
type Index  = Int
type Score  = Int

data NumbersAndBoards = NumbersAndBoards
    { numbers :: [Number]
    , boards  :: [[Number]]
    } deriving Show

-- 7,4,9,5,11,17,23,2,0,14,21,24
--
-- 22 13 17 11  0
--  8  2 23  4 24
-- 21  9 14 16  7
--  6 10  3 18  5
--  1 12 20 15 19
--
numbersAndBoards :: Parser NumbersAndBoards
numbersAndBoards =
    NumbersAndBoards
        <$> numbers <* eol <* eol
        <*> board `sepBy` eol
  where
    numbers = decimal `sepBy` char ','
    board   = join <$> (many1 (surroundedBy ' ' decimal) `endBy` eol)

ejectScoresAndRemaining :: (PrimV m s v a) => a -> [v s a] -> m ([Score], [v s a])
ejectScoresAndRemaining n =
    foldlM
        (\(ss, remBs) b -> markMaybeScore b n >>= \case
            Just s  -> return (ss <> [s], remBs)
            Nothing -> return (ss,        remBs <> [b]))
        ([], [])

markMaybeScore :: (PrimV m s v a) => v s a -> a -> m (Maybe Score)
markMaybeScore b n =
    G.ifoldM markMaybeIndex Nothing b >>= \case
        Nothing -> return Nothing
        Just i -> do
            colWon <- foldVector (.|.) 0 (rem  i 5)     (+5) 5 b
            rowWon <- foldVector (.|.) 0 (quot i 5 * 5) (+1) 5 b

            if rowWon == 0 || colWon == 0
                then Just . (* n2i n) <$> G.foldl' (\a x -> a + n2i x) 0 b
                else return Nothing
  where
    n2i = fromInteger . toInteger
    markMaybeIndex mi i x
        | x == n    = G.write b i 0 >> return (Just i)
        | otherwise = return mi

solve1 :: String -> IO Int
solve1 =
      (\NumbersAndBoards{numbers, boards} ->
          mapM (VU.thaw . VU.fromList) boards >>= go numbers)
    . parseOrDie numbersAndBoards
  where
    go []     _  = error "No winner found"
    go (n:ns) bs = ejectScoresAndRemaining n bs >>= \case
        ([],  remBs) -> go ns remBs     -- continue (no winners)
        ([s], _)     -> return s        -- stop on first score (1 winner)
        (ss,  _)     -> error $ "Expected a unique winner, but got: " <> show (length ss)

solve2 :: String -> IO Int
solve2 =
      (\NumbersAndBoards{numbers, boards} ->
          mapM (VU.thaw . VU.fromList) boards >>= go Nothing numbers)
    . parseOrDie numbersAndBoards
  where
    go _       []      _  = error "No winner found"
    go Nothing  _      [] = error "No winners or last single winner found"
    go (Just s) _      [] = return s
    go mw       (n:ns) bs = ejectScoresAndRemaining n bs >>= \case
        ([],  remBs) -> go mw       ns remBs    -- continue (no winners)
        ([s], remBs) -> go (Just s) ns remBs    -- set last score (1 winner)
        (_,   remBs) -> go Nothing  ns remBs    -- unset last score (2+ winners)

-- vector utils
foldVector :: forall m s v a b. (PrimV m s v a)
    => (b -> a -> b) -> b -> Index -> (Index -> Index) -> Int -> v s a -> m b
foldVector op initA initI moveI times v =
    go initA initI times
  where
    go :: b -> Int -> Int -> m b
    go a _ 0 = return a
    go a i n = do
        x <- G.read v i
        go (a `op` x) (moveI i) (n-1)
