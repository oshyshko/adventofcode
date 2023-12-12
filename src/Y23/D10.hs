module Y23.D10 where

import qualified Data.Map.Strict as M
import           Geom.XY
import           Imports
import           Util
import qualified Vec2            as V
import           Vec2            (Vec2)

type Cell  = Char
type Board = Vec2 Cell

data Dir = U | D | L | R deriving (Show, Eq, Ord)

parse :: String -> (Board, XY)                                              -- (board, start)
parse s =
    let board = V.fromList $ lines s
    in (board, fromJust $ V.findIndex (== 'S') board)

tryFourLoops :: Board -> XY -> [[(XY, Dir)]]
tryFourLoops board start =
    filter isLoop $ fmap walk [R,D,U,L,error "Ran out of directions. Invalid input?"]
  where
    isLoop :: [(XY,Dir)] -> Bool
    isLoop path =
        last path & \(xy,d) -> xy + dir2xy d == start
    walk :: Dir -> [(XY, Dir)]
    walk d =
          iterate nextLinkAndDir (start, Just d)
        & takeWhile (isJust . snd)
        & fmap (fmap fromJust)
    nextLinkAndDir :: (XY, Maybe Dir) -> (XY, Maybe Dir)
    nextLinkAndDir (_, Nothing) = error "Should never reach here"
    nextLinkAndDir (prevXy, Just moveDxy) =
        let currentXy = prevXy + dir2xy moveDxy
            nextDxy   = nextDelta moveDxy (board V.! currentXy)
        in (currentXy,nextDxy)
    nextDelta :: Dir -> Cell -> Maybe Dir
    nextDelta = \case                                                       -- prevDxy -> currentCell -> Maybe nextDxy
        U -> fmap ([L, R, U] !!) . (`elemIndex` "7F|")
        D -> fmap ([L, R, D] !!) . (`elemIndex` "JL|")
        L -> fmap ([D, U, L] !!) . (`elemIndex` "FL-")
        R -> fmap ([D, U, R] !!) . (`elemIndex` "7J-")
    dir2xy :: Dir -> XY
    dir2xy = \case
        U -> XY   0  (-1)
        D -> XY   0    1
        L -> XY (-1)   0
        R -> XY   1    0

solve1 :: String -> Int
solve1 = (`div` 2) . length . head . uncurry tryFourLoops . parse

solve2 :: String -> Int
solve2 s =
    let (board,start) = parse s
    in    tryFourLoops board start
        & head . filter cwLoop                                              -- affects `rewire` and `countHoriSpaceUD`
        & (\p -> rewireUD ((last . filter (`elem` [U,D]) . fmap snd) p) p)  -- pass last U/D
        & fmap (\(XY x y,d) -> (y, [(x,d)]))
        & M.elems . M.fromListWith (++)                                     -- group by row
        & fmap (sum . fmap countHoriSpaceUD . divvy2 1 . sortOn fst)        -- sort, then find dashes of space with sides: U <---> D
        & sum
  where
    cwLoop :: [(XY, Dir)] -> Bool
    cwLoop xs =
        sum (fmap turn2int . divvy2 1 $ fmap snd xs) > 0                     -- a clockwise loop has more right turns
      where
        turn2int :: (Dir, Dir) -> Int
        turn2int ab@(a,b)
            | ab `elem` [(U,R), (R,D), (D,L), (L,U)] =  1                   -- right turn
            | ab `elem` [(R,U), (D,R), (L,D), (U,L)] = -1                   -- left turn
            | a == b                                 =  0                   -- straight line
            | otherwise = error $ "Unexpected input: " ++ show a ++ ", " ++ show b

    rewireUD :: Dir -> [(XY, Dir)] -> [(XY, Dir)]                           -- replace Ls and Rs with preeceeding Us and Ds
    rewireUD _ (r@(_,U):xs) = r : rewireUD U xs
    rewireUD _ (r@(_,D):xs) = r : rewireUD D xs
    rewireUD d (  (p,_):xs) = (p,d) : rewireUD d xs
    rewireUD _ []           = []

    countHoriSpaceUD :: ((Int, Dir), (Int, Dir)) -> Int
    countHoriSpaceUD ((a,U),(b,D)) = b - a - 1
    countHoriSpaceUD _             = 0
