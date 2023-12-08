module Y21.D21 where

import qualified Data.Map.Strict as M

import           Imports
import           Parser
import           Util

data State = State
    { aLoc   :: Int      -- [1..10]
    , bLoc   :: Int      -- [1..10]
    , aScore :: Int      -- win on score >= 1000 (or >= 21)
    , bScore :: Int
    , aNext  :: Bool     -- player A moves first
    } deriving (Show, Ord, Eq)

-- Player 1 starting position: 1
-- Player 2 starting position: 10
playerLocations :: Parser (Int, Int)
playerLocations =
    (,) <$> (string "Player 1 starting position: " *> natural <* eol)
        <*> (string "Player 2 starting position: " *> natural)

tick :: Int -> State -> State
tick rollSum State{aLoc,bLoc,aScore,bScore,aNext} =
    let (loc,score) = if aNext then (aLoc,aScore) else (bLoc,bScore)
        locNew      = succ ((pred loc + rollSum) `rem` 10)
        scoreNew    = score + locNew
    in State
        { aLoc      = if     aNext then locNew   else aLoc
        , bLoc      = if not aNext then locNew   else bLoc
        , aScore    = if     aNext then scoreNew else aScore
        , bScore    = if not aNext then scoreNew else bScore
        , aNext     = not aNext
        }

solve1 :: String -> Int
solve1 s =
    let (a,b) = parseOrDie playerLocations s
    in fix3 0 (cycle [(1::Int)..100]) (State a b 0 0 True) f
  where
    f loop rollCount (r0:r1:r2:diceSeqRem) state@State{aScore,bScore}
        | aScore >= 1000 = rollCount * bScore
        | bScore >= 1000 = rollCount * aScore
        | otherwise      = loop (3 + rollCount) diceSeqRem (tick (r0 + r1 + r2) state)
    f _ _ _ _ = shouldNeverReachHere

solve2 :: String -> Int
solve2 s =
    let (a,b) = parseOrDie playerLocations s
    in suncurry max . ssnd $ fix2 (M.empty @State @(T2 Int Int)) (State a b 0 0 True)
        \loop s2ab state@State{aScore,bScore} ->            -- s2ab etc. -- memoization cache
            case s2ab M.!? state of
                Just ab -> T2 s2ab ab
                Nothing ->
                    if | aScore >= 21 -> T2 s2ab (T2 1 0)
                       | bScore >= 21 -> T2 s2ab (T2 0 1)
                       | otherwise ->
                              foldl' (f loop state) (T2 s2ab (T2 0 0)) rollSum2count
                            & \(T2 s2ab3 ab) -> T2 (M.insert state ab s2ab3) ab     -- memoize the result + return
  where
    f loop state (T2 s2ab (T2 a b)) (rollSum,times) =
        let (T2 s2ab2 (T2 a2 b2)) = loop s2ab (tick rollSum state)                  -- fan out + pass the cache
            playerAwins           = a + times * a2
            playerBwins           = b + times * b2
        in T2 s2ab2 (T2 playerAwins playerBwins)                                    -- return the cache + players' wins

    rollSum2count = M.toList $ M.fromListWith (+)
        [ (x + y + z, 1) | x <- [1..3], y <- [1..3], z <- [1..3] ]
