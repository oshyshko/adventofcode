{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Y15.D06 where

import           Control.Monad.ST             (ST, runST)
import           Data.Array.IO                (IOUArray)
import           Data.Array.MArray            (MArray)
import qualified Data.Array.MArray            as A
import           Data.Bit                     (Bit (..))
import qualified Data.HashMap.Strict          as MH
import qualified Data.IntMap.Strict           as MI
import qualified Data.Map.Strict              as MS
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM

import           Imports
import           Util

-- AI - Data.Array.IO.IOUArray
-- MH - Data.HashMap.Strict
-- MI - Data.IntMap.Strict
-- MS - Data.Map.Strict
-- VB - Data.VectorMutable + IO
-- VR - Data.Vector.Storable.Mutable + IO
-- VS - Data.Vector.Unboxed.Mutable  + ST
-- VU - Data.Vector.Unboxed.Mutable  + IO
-- VZ - Data.Vector.Unboxed.Mutable  + IO + Data.Bit

type Side       = Int
type Brightness = Word16
data Op         = On | Off | Toggle deriving Show
data XY         = XY Side Side      deriving Show

data Command = Command
    { op  :: Op
    , xy0 :: XY
    , xy1 :: XY
    } deriving Show

-- TODO find a way to migrate L1/L2 to a newtype + have MArray instances
type L1  = Bool
type L1B = Bit
type L2  = Word16

side :: Side -- TODO determine sides from input?
side = 1000

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
commands :: Parser [Command]
commands =
    command `endBy` eol
  where
    command :: Parser Command
    command = Command
        <$> op <* space
        <*> xy
        <* string " through " -- TODO many1 space ...
        <*> xy

    xy :: Parser XY
    xy = XY <$> decimal <* char ',' <*> decimal

    op :: Parser Op
    op =    try (string "turn on")  $> On
        <|> try (string "turn off") $> Off
        <|>      string "toggle"    $> Toggle

class Light a where
    initial    :: a
    brightness :: a -> Brightness
    operate    :: Op -> a -> a

instance Light L1 where
    initial     = False
    brightness  = bool 0 1
    operate     = \case
        On     -> const True
        Off    -> const False
        Toggle -> not

instance Light L1B where
    initial     = Bit False
    brightness  = bool 0 1 . unBit
    operate     = \case
        On     -> const (Bit True)
        Off    -> const (Bit False)
        Toggle -> Bit . not . unBit

instance Light L2 where
    initial     = 0
    brightness  = id
    operate     = \case
        On     -> (+1)
        Off    -> \v -> bool (v-1) 0 (v==0)
        Toggle -> (+2)

{-# INLINE command2indexes #-} -- note: otherwise it allocates 1.7GB for arr/vec
command2indexes :: Command -> [Side]
command2indexes (Command _ (XY x0 y0) (XY x1 y1)) =
    [ x * side + y
    | x <- [x0..x1]
    , y <- [y0..y1]
    ]

class StorageM s k v where
    emptySM :: s k v
    alterSM :: (v -> v) -> k -> s k v -> s k v
    foldlSM :: (a -> v -> a) -> a -> s k v -> a

class (PrimMonad m) => StorageI s v m where
    emptySI :: Int -> v -> m (s v)
    alterSI :: (v -> v) -> Int -> s v -> m ()
    foldlSI :: (a -> v -> a) -> a -> s v -> m a

{-# ANN solveM ("HLint: ignore Avoid lambda" :: String) #-}
solveM :: forall s v . (Light v, StorageM s Side v) => String -> Int
solveM input =
      parseOrDie commands input
    & foldl' applyCommand emptySM
    & foldlSM (\a v -> a + fromIntegral (brightness v)) 0
  where
    applyCommand :: s Side v -> Command -> s Side v
    applyCommand s' c@Command{op} =
        foldl' (\a v -> alterSM (operate op) v a) s' (command2indexes c)

solveI :: forall s v m. (Light v, PrimMonad m, StorageI s v m) => String -> m Int
solveI input = do
    s <- emptySI @s @v @m (side * side - 1) initial
    forM_ (parseOrDie commands input) $ applyCommand s
    foldlSI (\a v -> a + fromIntegral (brightness v)) 0 s
  where
    applyCommand s' c@Command{op} =
        forM_ (command2indexes c) (\k -> alterSI (operate op) k s')

-- solutions
solve1MH, solve2MH
    , solve1MS, solve2MS
    , solve1MI, solve2MI
    , solve1VS, solve2VS :: String -> Int

solve1, solve2
    , solve1AI, solve2AI
    , solve1VB, solve2VB
    , solve1VR, solve2VR
    , solve1VU, solve2VU
    , _solve1VZ :: String -> IO Int

solve1MH   =         solveM @MH.HashMap                        @L1
solve2MH   =         solveM @MH.HashMap                        @L2
solve1MS   =         solveM @MS.Map                            @L1
solve2MS   =         solveM @MS.Map                            @L2
solve1MI   =         solveM @IntMap'                           @L1
solve2MI   =         solveM @IntMap'                           @L2

solve1AI   =         solveI @(IOUArray Int)                    @L1 @IO
solve2AI   =         solveI @(IOUArray Int)                    @L2 @IO
solve1VB   =         solveI @(VM.MVector  (PrimState IO))      @L1 @IO
solve2VB   =         solveI @(VM.MVector  (PrimState IO))      @L2 @IO
solve1VR   =         solveI @(VSM.MVector (PrimState IO))      @L1 @IO
solve2VR   =         solveI @(VSM.MVector (PrimState IO))      @L2 @IO
solve1VS s = runST $ solveI @(VUM.MVector (PrimState (ST _)))  @L1 @(ST _) s
solve2VS s = runST $ solveI @(VUM.MVector (PrimState (ST _)))  @L2 @(ST _) s
solve1VU   =         solveI @(VUM.MVector (PrimState IO))      @L1 @IO
solve2VU   =         solveI @(VUM.MVector (PrimState IO))      @L2 @IO

-- TODO make it possible in MainExe to run one solver (day 1 or 2)
_solve1VZ   =        solveI @(VUM.MVector (PrimState IO))      @L1B @IO

-- best performance
solve1 = solve1VU
solve2 = solve2VU

-- instances: map-based sotrages
instance (Light v) => StorageM MS.Map Side v where
    emptySM = MS.empty
    alterSM = MS.alter . fromJustFold
    foldlSM = MS.foldl'

-- TODO find out why HashMap is slower than Map
-- also see https://github.com/haskell-perf/dictionaries
instance (Light v) => StorageM MH.HashMap Side v where
    emptySM = MH.empty
    alterSM = MH.alter . fromJustFold
    foldlSM = MH.foldl'

newtype IntMap' k v = IntMap' (MI.IntMap v) -- TODO get rid of this type?

instance (Light v) => StorageM IntMap' Side v where
    emptySM                 = IntMap' MI.empty
    alterSM f k (IntMap' m) = IntMap' $ MI.alter (fromJustFold f) k m
    foldlSM f a (IntMap' m) = MI.foldl' f a m

{-# INLINE fromJustFold #-}
fromJustFold :: (Light v) => (v -> v) -> Maybe v -> Maybe v
fromJustFold f mv =
    let x = f (fromMaybe initial mv)
    in  x `seq` Just x

-- instances: index-based storages
instance (m ~ IO, MArray IOUArray v m, A.Ix k, k ~ Int)
    => StorageI (IOUArray k) v m
  where
    emptySI   k v = A.newArray (0, k - 1) v
    alterSI f k s = A.readArray s k >>= A.writeArray s k . f
    foldlSI f a s = foldl' f a <$> A.getElems s

instance (PrimMonad m, st ~ PrimState m)
    => StorageI (VM.MVector st) v m
  where
    emptySI   k v = VM.replicate k v
    alterSI f k s = VM.modify s f k
    foldlSI f a s = V.foldl' f a <$> V.freeze s

instance (PrimMonad m, st ~ PrimState m, VUM.Unbox v)
    => StorageI (VUM.MVector st) v m
  where
    emptySI   k v = VUM.replicate k v
    alterSI f k s = VUM.modify s f k
    foldlSI f a s = VU.foldl' f a <$> VU.freeze s

instance (PrimMonad m, st ~ PrimState m, VSM.Storable v)
    => StorageI (VSM.MVector st) v m
  where
    emptySI   k v = VSM.replicate k v
    alterSI f k s = VSM.modify s f k
    foldlSI f a s = VS.foldl' f a <$> VS.freeze s
