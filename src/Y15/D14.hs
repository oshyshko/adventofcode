module Y15.D14 where

import           Data.List                     (sortOn, (!!))
import           Data.Ord                      (Down (..))
import           Text.ParserCombinators.Parsec (Parser, digit, endBy, letter,
                                                many, parse, string, try, (<|>))

type KmS     = Int
type Km      = Int
type Seconds = Int

data State = Running Seconds
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
        <$> many letter           <* string " can fly "
        <*> (read <$> many digit) <* string " km/s for "
        <*> (read <$> many digit) <* string " seconds, but then must rest for "
        <*> (read <$> many digit) <* string " seconds."

    eol :: Parser String
    eol =   try (string "\n\r")
        <|> try (string "\r\n")
        <|>      string "\n"
        <|>      string "\r"

tick :: Racer -> Racer
tick r = case state r of
    Running 1 -> r { state = Resting (restTime . spec $ r), distance = distance r + (speed . spec) r }
    Running n -> r { state = Running (n - 1),               distance = distance r + (speed . spec) r}
    Resting 1 -> r { state = Running (runTime . spec $ r) }
    Resting n -> r { state = Resting (n - 1)}

race :: Seconds -> [Racer] -> [Racer]
race n = head . drop n . iterate tickAll
  where
    tickAll :: [Racer] -> [Racer]
    tickAll rs = let ranRs@(lead:_) = sortOn (Down . distance) . map tick $ rs
                 in map (\r -> if distance lead == distance r
                                   then r { points = points r + 1}
                                   else r)
                        ranRs

solveBy :: (Racer -> Int) -> String -> Int
solveBy distanceOrPoints str = either
    (error . show)
    (distanceOrPoints
        . head
        . sortOn (Down . distanceOrPoints)
        . race 2503 -- 2503 seconds to race
        . map (\s -> Racer s (Running $ runTime s) 0 0))
    (parse specs "specs" str)

solve1 :: String -> Int
solve1 = solveBy distance

solve2 :: String -> Int
solve2 = solveBy points
