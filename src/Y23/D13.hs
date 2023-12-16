module Y23.D13 where

import           Geom.XY
import           Imports
import           Parser
import qualified Vec2    as V
import           Vec2    (Vec2 (..))

type Image = Vec2 Bit

-- #.##..##.
-- ..#.##.#.
-- ##......#
-- ##......#
-- ..#.##.#.
-- ..##..##.
-- #.#.##.#.
images :: Parser [Image]
images =
    (V.fromList <$> image) `sepBy` eol
  where
    image = line `endBy` eol
    line = many1 ((Bit True <$ char '#') <|> (Bit False <$ char '.'))

-- diifsVert (wh m) (m V.!) -> [2,11,13,16,0,11,8,7]
diffsVert :: Eq a => XY -> (XY -> a) -> [Int]
diffsVert (XY w h) getXY =
    foldl1' (zipWith (+)) $
        [0..h-1] <&> \y ->
            [1..w-1] <&> \x ->
                let size = 1 + min (x-1) (w-1-x)
                in sum $ [0..size-1] <&> \i ->
                    bool 0 1 $ getXY (XY (x-1-i) y) /= getXY (XY (x+i) y)

solve :: Int -> String -> Int
solve targetDiff s =
      parseOrDie images s
    & concatMap diifsHoriVert
    & partitionEithers
    & \(cols,rows) -> 100 * sum rows + sum cols
  where
    diifsHoriVert :: Image -> [Either Int Int]          -- Either Cols Rows
    diifsHoriVert m = catMaybes
         [ fmap (Left  . (1 +)) . elemIndex targetDiff $ diffsVert (         wh m) ( m V.!          )
         , fmap (Right . (1 +)) . elemIndex targetDiff $ diffsVert (xySwap $ wh m) ((m V.!) . xySwap)
         ]

solve1, solve2 :: String -> Int
solve1 = solve 0
solve2 = solve 1
