module Y15.D14 where

import           Data.Ord (Down (..))

import           Imports
import           Util

type KmS     = Int
type Km      = Int
type Seconds = Int

data State
    = Running Seconds
    | Resting Seconds
    deriving Show

data Spec = Spec
    { name     :: String
    , speed    :: KmS
    , runTime  :: Seconds
    , restTime :: Seconds } deriving Show

data Racer = Racer
    { spec     :: Spec
    , state    :: State
    , points   :: Int
    , distance :: Km } deriving Show

-- Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
-- Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
specs :: Parser [Spec]
specs =
    _spec `endBy` eol
  where
    _spec :: Parser Spec
    _spec = Spec
        <$> many letter <* string " can fly "
        <*> decimal     <* string " km/s for "
        <*> decimal     <* string " seconds, but then must rest for "
        <*> decimal     <* string " seconds."

tick :: Racer -> Racer
tick r@Racer{state} = case state of
    Running 1 -> r { state = Resting (restTime . spec $ r), distance = distance r + (speed . spec) r }
    Running n -> r { state = Running (n - 1),               distance = distance r + (speed . spec) r}
    Resting 1 -> r { state = Running (runTime . spec $ r) }
    Resting n -> r { state = Resting (n - 1)}

race :: Seconds -> [Racer] -> [Racer]
race n racers =
    iterate tickAll racers !! n
  where
    tickAll :: [Racer] -> [Racer]
    tickAll rs =
        let ranRs@(lead:_) = sortOn (Down . distance) . map tick $ rs
        in ranRs <&> \r ->
            if distance lead == distance r
                then r { points = points r + 1}
                else r

solveBy :: (Racer -> Int) -> String -> Int
solveBy distanceOrPoints input =
      parseOrDie specs input
    & map (\s -> Racer s (Running $ runTime s) 0 0)
    & race 2503 -- 2503 seconds to race
    & sortOn (Down . distanceOrPoints)
    & head
    & distanceOrPoints

solve1 :: String -> Int
solve1 = solveBy distance

solve2 :: String -> Int
solve2 = solveBy points
