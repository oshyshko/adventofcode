{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Y15.D06 where

import           Control.Monad                 (forM_)
import           Control.Monad.Primitive       (PrimMonad, PrimState)
import           Control.Monad.ST              (RealWorld, ST, runST)
import           Data.Array.IO                 (IOUArray)
import           Data.Array.MArray             (MArray, getElems, newArray,
                                                readArray, writeArray)
import           Data.Array.ST                 (STUArray)
import           Data.Foldable                 (foldl')
import           Data.Functor                  (($>))
import qualified Data.HashMap.Strict           as MH
import           Data.Int                      (Int16)
import qualified Data.IntMap.Strict            as MI
import qualified Data.Map.Strict               as MS
import           Data.Maybe                    (fromMaybe)
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed.Mutable   (MVector, Unbox)
import qualified Data.Vector.Unboxed.Mutable   as VM
-- import           Data.Word                     (Word32)
import           Text.ParserCombinators.Parsec (Parser, char, digit, endBy,
                                                many, space, string, try, (<|>))
import           Util

-- AI -- Data.Array.IO.IOUArray
-- AS -- Data.Array.ST.STUArray
-- MH -- Data.HashMap.Strict
-- MI -- Data.IntMap.Strict
-- MS -- Data.Map.Strict
-- VI -- Data.Vector.Unboxed.Mutable.MVector + IO
-- VS -- Data.Vector.Unboxed.Mutable.MVector + ST

type Side       = Int -- Word32
type Brightness = Int -- Int32
data Op         = On | Off | Toggle deriving Show

data Command = Command
             { op ::Op
             , x0 :: Side
             , y0 :: Side
             , x1 :: Side
             , y1 :: Side
             } deriving Show

-- TODO find a way to migrate L1/L2 to a newtype + have MArray instances
type L1 = Bool
type L2 = Int16

side :: Side -- TODO determine sides from input?
side = 1000

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
commands :: Parser [Command]
commands = command `endBy` eol
  where
    command :: Parser Command
    command = Command
        <$> op <* space
        <*> (read <$> many digit) <* char ','
        <*> (read <$> many digit)
        <* string " through " -- TODO many1 space ...
        <*> (read <$> many digit) <* char ','
        <*> (read <$> many digit)

    op :: Parser Op
    op =    try (string "turn on")  $> On
        <|> try (string "turn off") $> Off
        <|>      string "toggle"    $> Toggle

class Light a where
    initial    :: a
    brightness :: a -> Brightness
    operate    :: a -> Op -> a

instance Light L1 where
    initial      = False
    brightness v = if v then 1 else 0
    operate v    = \case
        On     -> True
        Off    -> False
        Toggle -> not v

instance Light L2 where
    initial      = 0
    brightness   = fromIntegral
    operate v    = \case
        On     -> v + 1
        Off    -> max 0 (v - 1)
        Toggle -> v + 2

-- map based implementations
class Storage s k v where
    empty  :: s k v
    alter  :: (Maybe v -> Maybe v) -> k -> s k v -> s k v
    foldlS :: (a -> v -> a) -> a -> s k v -> a

instance Storage MS.Map Side v where
    empty  = MS.empty
    alter  = MS.alter
    foldlS = MS.foldl'

instance Storage MH.HashMap Side v where
    empty  = MH.empty
    alter  = MH.alter
    foldlS = MH.foldl'

newtype IntMap' k v = IntMap (MI.IntMap v) -- TODO get rid of this phantom type
instance Storage IntMap' Side v where
    empty                 = IntMap MI.empty
    alter  f k (IntMap m) = IntMap $ MI.alter f k m
    foldlS f i (IntMap m) = MI.foldl' f i m

command2indexes :: Command -> [Side]
command2indexes Command{..} =
    [x * side + y | x <- [x0..x1]
                  , y <- [y0..y1]]

applyCommandsAndSum :: (Light v, Storage s Side v) => s Side v -> [Command] -> Brightness
applyCommandsAndSum s cs =
    getSum $ foldl' applyCommand s cs
  where
    getSum :: (Light v, Storage s Side v) => s Side v -> Brightness
    getSum = foldlS (\a v -> a + brightness v) 0

    applyCommand :: (Light v, Storage s Side v) => s Side v -> Command -> s Side v
    applyCommand ss c =
        foldl' (flip $ alter (\mv ->
                                let x = operate (fromMaybe initial mv) (op c)
                                in  x `seq` Just x))
            ss
            (command2indexes c)

solve1MH, solve2MH :: String -> Brightness
solve1MH = applyCommandsAndSum (empty :: MH.HashMap Side L1) . parseOrDie commands
solve2MH = applyCommandsAndSum (empty :: MH.HashMap Side L2) . parseOrDie commands

solve1MS, solve2MS :: String -> Brightness
solve1MS = applyCommandsAndSum (empty :: MS.Map Side L1) . parseOrDie commands
solve2MS = applyCommandsAndSum (empty :: MS.Map Side L2) . parseOrDie commands

solve1MI, solve2MI :: String -> Brightness
solve1MI = applyCommandsAndSum (empty :: IntMap' Side L1) . parseOrDie commands
solve2MI = applyCommandsAndSum (empty :: IntMap' Side L2) . parseOrDie commands

-- array based implementations
applyCommandsAndSumArray :: (Light e, MArray a e m) => [Command] -> a Side e -> m Brightness
applyCommandsAndSumArray cs arr = do
    forM_ cs $ applyCommand arr
    sum . map brightness <$> getElems arr
  where
    applyCommand :: (Light e, MArray a e m) => a Side e -> Command -> m ()
    applyCommand a c =
        forM_
            (command2indexes c)
            (\i -> do
                v <- readArray a i
                writeArray a i (operate v $ op c))

newArr :: (Light e, MArray a e m) => m (a Side e)
newArr = newArray (0, side * side - 1) initial

solve1AI, solve2AI :: String -> IO Brightness
solve1AI s = applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: IO (IOUArray Side L1))
solve2AI s = applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: IO (IOUArray Side L2))

solve1AS, solve2AS :: String -> Brightness
solve1AS s = runST $ applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: ST s (STUArray s Side L1))
solve2AS s = runST $ applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: ST s (STUArray s Side L2))

-- vector based implementations
-- see https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial
-- see https://tech.fpcomplete.com/haskell/library/vector
newVec :: (Light e, PrimMonad m, Unbox e) => m (MVector (PrimState m) e)
newVec = VM.replicate (side * side - 1) initial

applyCommandsAndSumVector :: (Light e, PrimMonad m, Unbox e) => [Command] -> MVector (PrimState m) e -> m Brightness
applyCommandsAndSumVector cs vv = do
    forM_ cs $ applyCommand vv
    V.foldl' (\a x -> a + brightness x) 0 <$> V.freeze vv
  where
    applyCommand :: (Light e, PrimMonad m, Unbox e) => MVector (PrimState m) e -> Command -> m ()
    applyCommand v c = forM_ (command2indexes c) (VM.modify v (`operate` op c))

solve1VI, solve2VI :: String -> IO Brightness
solve1VI s = applyCommandsAndSumVector (parseOrDie commands s) =<< (newVec :: IO (MVector RealWorld L1))
solve2VI s = applyCommandsAndSumVector (parseOrDie commands s) =<< (newVec :: IO (MVector RealWorld L2))

solve1VS, solve2VS :: String -> Brightness
solve1VS s = runST $ applyCommandsAndSumVector (parseOrDie commands s) =<< (newVec :: ST s (MVector s L1))
solve2VS s = runST $ applyCommandsAndSumVector (parseOrDie commands s) =<< (newVec :: ST s (MVector s L2))
