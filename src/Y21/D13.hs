module Y21.D13 where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Imports
import           Util

type YX   = (Int, Int)
data Fold = AlongX Int | AlongY Int deriving Show

-- 6,10
-- 0,14
--
-- fold along y=7
-- fold along x=5
--
dotsAndFolds :: Parser ([YX], [Fold])
dotsAndFolds =
    (,) <$> (xy `endBy` eol) <* eol
        <*> (fold `endBy` eol)
  where
    xy :: Parser YX
    xy = flip (,) <$> natural <* char ',' <*> natural
    fold :: Parser Fold
    fold = do
        string "fold along "
        let fx = char 'x' $> AlongX
            fy = char 'y' $> AlongY
        (fx <|> fy) <*> (char '=' *> natural)

foldOnce :: [YX] -> Fold -> [YX]
foldOnce dots fold =
    let wrap = case fold of
            AlongX alongX -> \yx@(y,x) -> if x > alongX then (y, 2*alongX-x) else yx
            AlongY alongY -> \yx@(y,x) -> if y > alongY then (2*alongY-y, x) else yx
    in fmap wrap dots

--  ##    ## #  #  ##  #### #  # #  # #  #
-- #  #    # #  # #  #    # #  # # #  #  #
-- #       # #### #  #   #  #### ##   #  #
-- #       # #  # ####  #   #  # # #  #  #
-- #  # #  # #  # #  # #    #  # # #  #  #
--  ##   ##  #  # #  # #### #  # #  #  ##
--
showDots :: [YX] -> String
showDots yxs =
    let maxY = maximum . fmap fst $ yxs
        maxX = maximum . fmap snd $ yxs
        s    = S.fromList yxs
    in unlines $ flip fmap [0..maxY] \y ->
        flip fmap [0..maxX] \x ->
            bool ' ' '#' (S.member (y,x) s)

solve1 :: String -> Int
solve1 =
      length
    . nub
    . (\(dots, folds) -> foldl' foldOnce dots [head folds])
    . parseOrDie dotsAndFolds

solve2 :: String -> String
solve2 =
      fmap ((pat2char M.!) . (concat . take 4))
    . divvy 4 5
    . transpose
    . lines
    . showDots
    . uncurry (foldl' foldOnce)
    . parseOrDie dotsAndFolds
  where
    pat2char :: Map String Char
    pat2char =
        M.fromList $ zip
            (fmap concat $ divvy 4 5 $ transpose $ drop 1 rawChars)
            (fmap head $ divvy 1 5 $ head rawChars)
    rawChars :: [String]
    rawChars =
        [ "A    B    C    E    F    G    H    J    K    L    P    R    U    Z   "
        , " ##  ###   ##  #### ####  ##  #  #   ## #  # #    ###  ###  #  # ####"
        , "#  # #  # #  # #    #    #  # #  #    # # #  #    #  # #  # #  #    #"
        , "#  # ###  #    ###  ###  #    ####    # ##   #    #  # #  # #  #   # "
        , "#### #  # #    #    #    # ## #  #    # # #  #    ###  ###  #  #  #  "
        , "#  # #  # #  # #    #    #  # #  # #  # # #  #    #    # #  #  # #   "
        , "#  # ###   ##  #### #     ### #  #  ##  #  # #### #    #  #  ##  ####"
        ]
