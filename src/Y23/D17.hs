module Y23.D17 where

import           Geom.XY
import           Imports
import qualified Pathfinder       as P
import qualified Vec2             as V
import           Vec2             (Vec2 (..))

-- NOTE: imports for visualization
import qualified Data.HashPSQ     as QH
import qualified Data.IORef       as R
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import           Pathfinder       (Queue (..), Storage (..))
import           System.IO.Unsafe (unsafePerformIO)
import qualified Terminal         as T

data Node = Node
    { pos :: XY
    , dir :: XY
    } deriving (Eq, Ord, Generic, Show)

instance Hashable Node

-- 241343
-- 321545
-- 325524
parse :: String -> Vec2 Word8
parse = V.fromList . fmap (fmap $ fromIntegral . digitToInt) . lines

mkConfig :: (Int,Int) -> String -> (Vec2 Word8, P.Config Node Word16)
mkConfig (minMove,maxMove) s =
    let v2@Vec2{wh} = parse s
        goal = (wh - 1)
    in (v2, P.Config
            { neighbors = \Node{pos,dir} ->
                [ Node n d
                | r <- [minMove..maxMove]
                , d <- udlr
                , d /= dir && d /= negate dir
                , let n = pos + d * XY r r
                , n `V.within` v2
                ]
            , cost = \(Node a _) (Node z _) ->
                let dp = signum $ a - z
                in sum . fmap (fromIntegral . (v2 V.!)) . takeWhile (/= a) $ iterate (+ dp) z

            , remaining = Just \(Node{pos}) -> fromIntegral $ distanceManhattan goal pos
            , start     = Node 0 0
            , goal      = \Node{pos} -> pos == goal
            })

solve1, solve2 :: String -> Int
solve1 = fromIntegral . fromJust . P.minScore . snd . mkConfig (1,3)
solve2 = fromIntegral . fromJust . P.minScore . snd . mkConfig (4,10)

-- NOTE: visualization
visualize :: IO ()
{-# NOINLINE visualize #-}
visualize = do
    let example = unlines
            [ "2413432311323", "3215453535623", "3255245654254", "3446585845452", "4546657867536"
            , "1438598798454", "4457876987766", "3637877979653", "4654967986887", "4564679986453"
            , "1224686865563", "2546548887735", "4322674655533" ]
    let (v2,config) = mkConfig (1,3) example
    T.clearScreen
    print $ P.minPathTrace (traceStep v2 (unsafePerformIO $ R.newIORef 0)) config
    pure ()
  where
    traceStep
        :: forall q s p c. (p ~ Node, c ~ Word16, q ~ QH.HashPSQ p c (), s ~ Map p (p,c))
        => Vec2 Word8 -> R.IORef Int -> Bool -> p -> [p] -> [p] -> q -> s -> ()
    traceStep Vec2{wh} rstep _goalReached cp ns path q s =
        unsafePerformIO . {- when goalReached . -} T.withTerminal $ do
            let XY w h = wh

            step <- R.readIORef rstep
            R.writeIORef rstep (step + 1)

            T.setCursorPosition 0 0

            let xy2sCount = sToList @s @p @c @(p,c) s & fmap ((,1::Int). pos . fst) & M.fromListWith (+)
            let xy2qSet      = S.fromList . fmap pos $ qToList @q @p @c q

            forM_ [0..h-1] $ \y -> do
                T.setCursorPosition y 0

                forM_ [0..w-1] $ \x -> do
                    let xy = XY x y
                        count   = fromMaybe 0 $ xy2sCount M.!? xy
                        qm      = xy `S.member` xy2qSet
                        s4      = count == 4
                        isPath  = any (\(Node p _) -> p == xy) path
                        n       = (any $ \(Node p' _) -> p' == xy) ns
                        -- c       = intToDigit . fromIntegral $ v2 V.! xy
                        c = intToDigit count
                    -- see https://en.wikipedia.org/wiki/ANSI_escape_code
                    putStr $
                        if  | isPath        -> " \ESC[93m"          ++ [c] ++ "\ESC[0m" -- path (yellow)
                            | xy == pos cp  -> " \ESC[31m\ESC[100m" ++ [c] ++ "\ESC[0m" -- current
                            | n             -> " \ESC[32m\ESC[100m" ++ [c] ++ "\ESC[0m" -- neighbor (green, gray back)
                            | qm            -> " \ESC[32m"          ++ [c] ++ "\ESC[0m" -- open (green)
                            | s4            -> " \ESC[31m"          ++ [c] ++ "\ESC[0m" -- closed (red)
                            | otherwise     -> " "                  ++ [c]              -- white

            putStrLn ""
            putStrLn ""; putStrLn $ "step: "    ++ show step
            -- putStrLn ""; putStrLn $ "ns: "      ++ show ns
            -- putStrLn ""; putStrLn $ "q: "       ++ show q
            -- putStrLn ""; putStrLn $ "s: "       ++ show s
            -- putStrLn ""; putStrLn $ "path: "    ++ show path
            putStrLn ""

            -- _t <- T.getKey
            -- T.clearScreen

            pure ()
