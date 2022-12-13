module Y21.D13 where

import qualified Data.Set as S

import           Imports
import qualified Letters  as L
import           Parser
import           XY

data Fold = AlongX Int | AlongY Int deriving Show

-- 6,10
-- 0,14
--
-- fold along y=7
-- fold along x=5
--
dotsAndFolds :: Parser ([XY], [Fold])
dotsAndFolds =
    (,) <$> (xy `endBy` eol) <* eol
        <*> (fold `endBy` eol)
  where
    xy :: Parser XY
    xy =  XY <$> natural <* char ',' <*> natural
    fold :: Parser Fold
    fold = do
        string "fold along "
        let fx = char 'x' $> AlongX
            fy = char 'y' $> AlongY
        (fx <|> fy) <*> (char '=' *> natural)

foldOnce :: [XY] -> Fold -> [XY]
foldOnce dots fold =
    let wrap = case fold of
            AlongX alongX -> \xy@(XY x y) -> if x > alongX then XY (2*alongX-x) y else xy
            AlongY alongY -> \xy@(XY x y) -> if y > alongY then XY x( 2*alongY-y) else xy
    in fmap wrap dots

--  ##    ## #  #  ##  #### #  # #  # #  #
-- #  #    # #  # #  #    # #  # # #  #  #
-- #       # #### #  #   #  #### ##   #  #
-- #       # #  # ####  #   #  # # #  #  #
-- #  # #  # #  # #  # #    #  # # #  #  #
--  ##   ##  #  # #  # #### #  # #  #  ##
--
showDots :: [XY] -> String
showDots xys =
    let maxX = maximum . fmap getX $ xys
        maxY = maximum . fmap getY $ xys
        s    = S.fromList xys
    in unlines $ flip fmap [0..maxY] \y ->
        flip fmap [0..maxX] \x ->
            bool ' ' '#' (S.member (XY x y) s)

solve1 :: String -> Int
solve1 =
      length
    . nub
    . (\(dots, folds) -> foldl' foldOnce dots [head folds])
    . parseOrDie dotsAndFolds

solve2 :: String -> String
solve2 =
      L.parse
    . lines
    . showDots
    . uncurry (foldl' foldOnce)
    . parseOrDie dotsAndFolds
