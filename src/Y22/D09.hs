module Y22.D09 where

import qualified Data.Set as S
import           Imports
import           Parser
import           XY

moves :: Parser [(XY, Int)]
moves =
    move `endBy` eol
  where
    move = (,) <$> dxy <* pad <*> natural
    dxy =   char 'U' $> XY   0 (-1)
        <|> char 'D' $> XY   0   1
        <|> char 'L' $> XY (-1)  0
        <|> char 'R' $> XY   1   0

iterations :: Int -> [(XY, Int)] -> [[XY]]
iterations ropeLength =
      scanl (flip applyMove) (replicate ropeLength 0)
    . concatMap (\(dxy, n) -> replicate n dxy)
  where
    applyMove :: XY -> [XY] -> [XY]
    applyMove dxy = \case
        []     -> error "Got empty rope"
        (h:ts) -> let h' = h + dxy in h' : reweave h' ts
    reweave :: XY -> [XY] -> [XY]
    reweave h = \case
        []     -> []
        (t:ts) -> let t' = follow h t in t' : reweave t' ts
    follow :: XY -> XY -> XY
    follow h t =
        let (XY x y) = abs $ h - t
        in t + if x + y > 2 || (x + y == 2 && x * y == 0)
            then signum (h - t)
            else 0

solve1, solve2 :: String -> Int
solve1 = S.size . S.fromList . fmap (!! 1) . iterations 2  . parseOrDie moves
solve2 = S.size . S.fromList . fmap (!! 9) . iterations 10 . parseOrDie moves
