{-# LANGUAGE DefaultSignatures, DeriveAnyClass,
             DerivingStrategies, StandaloneDeriving #-}
module Y15.D06 where

import qualified Data.Array.Base              as AB
import           Data.Array.IO                (IOUArray)
import           Data.Array.MArray            (MArray)
import           Data.Bit                     (Bit (..))
import qualified Data.HashMap.Strict          as MH
import qualified Data.IntMap.Strict           as MI
import qualified Data.Map.Strict              as MS
import qualified Data.Vector.Generic.Mutable  as VG
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed.Mutable  as VUM

import           Imports
import           Util

-- MA - Data.Array.IO.IOUArray
-- MB - Data.Vector.Mutable (boxed)
-- MS - Data.Vector.Storable.Mutable
-- MU - Data.Vector.Unboxed.Mutable
-- MZ - Data.Vector.Unboxed.Mutable + Data.Bit
-- PH - Data.HashMap.Strict
-- PI - Data.IntMap.Strict
-- PS - Data.Map.Strict

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

class StorageMonadic s v m where
    emptySM :: Side -> v -> m s
    alterSM :: s -> (v -> v) -> Side -> m ()
    foldlSM :: (a -> v -> a) -> a -> s -> m a

    -- NOTE see 3 instances below
    default emptySM :: (vec x v ~ s, PrimMonad m, x ~ PrimState m, VG.MVector vec v) => Side -> v -> m s
    emptySM = VG.replicate

    default alterSM :: (vec x v ~ s, PrimMonad m, x ~ PrimState m, VG.MVector vec v) => s -> (v -> v) -> Side -> m ()
    alterSM = VG.modify

    default foldlSM :: (vec x v ~ s, PrimMonad m, x ~ PrimState m, VG.MVector vec v) => (a -> v -> a) -> a -> s -> m a
    foldlSM = VG.foldl'

class StoragePure s v where
    emptySP :: s
    alterSP :: (v -> v) -> Side -> s -> s
    foldlSP :: (a -> v -> a) -> a -> s -> a

{-# INLINE solvePure #-}
{-# ANN solvePure ("HLint: ignore Avoid lambda" :: String) #-}
solvePure :: forall s v. (Light v, StoragePure s v) => String -> Int
solvePure =
      foldlSP @s @v (\a v -> a + fromIntegral (brightness v)) 0 -- fold
    . foldl' applyCommand (emptySP @s @v)                       -- create, iterate + alter
    . parseOrDie commands
  where
    applyCommand :: s -> Command -> s
    applyCommand s c@Command{op} =
        foldl' (\a v -> alterSP @s @v (operate op) v a) s (command2indexes c)

{-# INLINE solveMonadic #-}
{-# ANN solveMonadic ("HLint: ignore Avoid lambda" :: String) #-}
solveMonadic :: forall s v m. (Light v, Monad m, StorageMonadic s v m) => String -> m Int
solveMonadic input = do
    s <- emptySM @s @v (side * side) initial                    -- create
    forM_ (parseOrDie commands input) (applyCommand s)          -- iterate + alter
    foldlSM @s @v (\a v -> a + fromIntegral (brightness v)) 0 s -- fold
  where
    applyCommand :: s -> Command -> m ()
    applyCommand s c@Command{op}=
            forM_ (command2indexes c)
                (\k -> alterSM @s @v s (operate op) k)

-- solutions
solve1PH, solve2PH
    , solve1PS, solve2PS
    , solve1PI, solve2PI
    :: String -> Int

solve1, solve2
    , solve1MA, solve2MA
    , solve1MB, solve2MB
    , solve1MS, solve2MS
    , solve1MU, solve2MU
    , zolve1MZ
    :: String -> IO Int

solve1   = solve1MU -- best performance
solve2   = solve2MU
solve1PH = solvePure    @(MH.HashMap Side L1)            @L1
solve2PH = solvePure    @(MH.HashMap Side L2)            @L2
solve1PS = solvePure    @(MS.Map     Side L1)            @L1
solve2PS = solvePure    @(MS.Map     Side L2)            @L2
solve1PI = solvePure    @(MI.IntMap       L1)            @L1
solve2PI = solvePure    @(MI.IntMap       L2)            @L2
solve1MA = solveMonadic @(IOUArray   Side L1)            @L1
solve2MA = solveMonadic @(IOUArray   Side L2)            @L2
solve1MB = solveMonadic @(VM.MVector  (PrimState IO) L1) @L1 @IO
solve2MB = solveMonadic @(VM.MVector  (PrimState IO) L2) @L2 @IO
solve1MS = solveMonadic @(VSM.MVector (PrimState IO) L1) @L1 @IO
solve2MS = solveMonadic @(VSM.MVector (PrimState IO) L2) @L2 @IO
solve1MU = solveMonadic @(VUM.MVector (PrimState IO) L1) @L1 @IO
solve2MU = solveMonadic @(VUM.MVector (PrimState IO) L2) @L2 @IO

-- TODO make it possible in MainExe to run one solver (day 1 or 2)
zolve1MZ = solveMonadic @(VUM.MVector (PrimState IO) L1B) @L1B @IO


-- monadic instances
instance (Monad m, MArray IOUArray v m, k ~ Side) => StorageMonadic (IOUArray k v) v m where
    emptySM k     = AB.newArray (0, k - 1)
    alterSM s f k = AB.unsafeRead s k >>= AB.unsafeWrite s k . f
    foldlSM f a s = AB.getNumElements s >>= \n -> go a (n-1)
      where
        go !aa 0 = return aa
        go !aa i = do
            x <- AB.unsafeRead s i
            go (f aa x) (i-1)

-- TODO figure hout how this works. See 3 defaults in StorageMonadic + language extensions at the top
deriving anyclass instance (PrimMonad m, s ~ PrimState m)                 => StorageMonadic (VM.MVector s v)  v m
deriving anyclass instance (PrimMonad m, s ~ PrimState m, VSM.Storable v) => StorageMonadic (VSM.MVector s v) v m
deriving anyclass instance (PrimMonad m, s ~ PrimState m, VUM.Unbox v)    => StorageMonadic (VUM.MVector s v) v m

-- pure instances
instance (Light v) => StoragePure (MS.Map Side v) v where
    emptySP = MS.empty;         alterSP = mkAlter MS.alter;     foldlSP = MS.foldl'

-- TODO find out why HashMap is slower than Map
-- also see https://github.com/haskell-perf/dictionaries
instance (Light v) => StoragePure (MH.HashMap Side v) v where
    emptySP = MH.empty;         alterSP = mkAlter MH.alter;     foldlSP = MH.foldl'

instance (Light v) => StoragePure (MI.IntMap v) v where
    emptySP = MI.empty;         alterSP = mkAlter MI.alter;     foldlSP = MI.foldl'

{-# INLINE mkAlter #-}
mkAlter :: forall k v m. (Light v) =>
    ((Maybe v -> Maybe v) -> k -> m v -> m v)
        -> (v -> v) -> k -> m v -> m v
mkAlter a f = a \mv ->
    let x = f $ fromMaybe initial mv
    in  x `seq` Just x
