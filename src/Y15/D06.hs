{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Y15.D06 where

import           Control.Monad                 (forM_)
import           Control.Monad.ST              (ST, runST)
import           Data.Array.IO                 (IOUArray)
import           Data.Array.MArray             (MArray, getElems, newArray,
                                                readArray, writeArray)
import           Data.Array.ST                 (STUArray)
import           Data.Foldable                 (foldl')
import           Data.Functor                  (($>))
import qualified Data.HashMap.Strict           as HM
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as SM
import           Data.Maybe                    (fromMaybe)
import           Text.ParserCombinators.Parsec (Parser, char, digit, endBy,
                                                many, space, string, try, (<|>))
import           Util

type Side       = Int -- Word32
type Brightness = Int -- Int32
data Op         = On | Off | Toggle deriving Show
type Command    = (Op, (Side, Side), (Side, Side))

-- TODO find a way to migrate L1/L2 to a newtype + have MArray instances
type L1 = Bool
type L2 = Int -- Int16

side :: Side -- TODO determine sides from input?
side = 1000

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
commands :: Parser [Command]
commands = command `endBy` eol
  where
    command :: Parser Command
    command = (,,) <$> op <* space
                   <*> xy <* string " through " -- TODO many1 space ...
                   <*> xy

    op :: Parser Op
    op =    try (string "turn on")  $> On
        <|> try (string "turn off") $> Off
        <|>      string "toggle"    $> Toggle

    xy :: Parser (Side, Side)
    xy = (,) <$> (read <$> many digit)
             <* char ','
             <*> (read <$> many digit)

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
    brightness v = v
    operate v    = \case
        On     -> v + 1
        Off    -> max 0 (v - 1)
        Toggle -> v + 2

-- map based implementations
class Storage s k v where
    empty  :: s k v
    alter  :: (Maybe v -> Maybe v) -> k -> s k v -> s k v
    foldlS :: (a -> v -> a) -> a -> s k v -> a
    elems  :: s k v -> [v]

instance Storage SM.Map Side v where
    empty  = SM.empty
    alter  = SM.alter
    foldlS = SM.foldl'
    elems  = SM.elems

instance Storage HM.HashMap Side v where
    empty  = HM.empty
    alter  = HM.alter
    foldlS = HM.foldl'
    elems  = HM.elems

newtype IntMap' k v = IntMap (IM.IntMap v) -- TODO get rid of this phantom type
instance Storage IntMap' Side v where
    empty                 = IntMap IM.empty
    alter  f k (IntMap m) = IntMap $ IM.alter f k m
    foldlS f i (IntMap m) = IM.foldl' f i m
    elems      (IntMap m) = IM.elems m


applyCommandsAndSum :: (Light v, Storage s Side v) => s Side v -> [Command] -> Brightness
applyCommandsAndSum s cs =
    getSum $ foldl' applyCommand s cs
  where
    getSum :: (Light v, Storage s Side v) => s Side v -> Brightness
    getSum = foldlS (\a v -> a + brightness v) 0

    applyCommand :: (Light v, Storage s Side v) => s Side v -> Command -> s Side v
    applyCommand ss (op, (x0,y0), (x1,y1)) =
        foldl' (flip $ alter (\mv ->
                                let x = operate (fromMaybe initial mv) op
                                in  x `seq` Just x))
            ss
            [x * side + y | x <- [x0..x1]
                          , y <- [y0..y1]]

solve1MH, solve2MH :: String -> Brightness
solve1MH = applyCommandsAndSum (empty :: HM.HashMap Side L1) . parseOrDie commands
solve2MH = applyCommandsAndSum (empty :: HM.HashMap Side L2) . parseOrDie commands

solve1MS, solve2MS :: String -> Brightness
solve1MS = applyCommandsAndSum (empty :: SM.Map Side L1) . parseOrDie commands
solve2MS = applyCommandsAndSum (empty :: SM.Map Side L2) . parseOrDie commands

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
    applyCommand a (op, (x0,y0), (x1,y1)) =
        forM_
            [x * side + y | x <- [x0..x1]
                          , y <- [y0..y1]]
            (\i -> do

                v <- readArray a i
                writeArray a i (operate v op))

newArr :: (Light e, MArray a e m) => m (a Side e)
newArr = newArray (0, side * side - 1) initial

solve1AI, solve2AI :: String -> IO Brightness
solve1AI s = applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: IO (IOUArray Side L1))
solve2AI s = applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: IO (IOUArray Side L2))

solve1AS, solve2AS :: String -> Brightness
solve1AS s = runST $ applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: ST s (STUArray s Side L1))
solve2AS s = runST $ applyCommandsAndSumArray (parseOrDie commands s) =<< (newArr :: ST s (STUArray s Side L2))
