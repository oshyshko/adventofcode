module Y15.D06HM where

import           Data.Foldable   (foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Util
import           Y15.D06Shared

type Matrix  = M.Map XY Int

-- TODO refactor to use D06Shared.applyCommand
applyCommandHM :: (Op -> Int -> Int) -> Matrix -> Command -> Matrix
applyCommandHM f mm (op, (x0,y0), (x1,y1)) =
    foldl' (\ m xy -> M.insert xy (f op (fromMaybe 0 $ M.lookup xy m)) m)
           mm
           [ (x,y) | x <- [x0..x1],
                     y <- [y0..y1]]

sumApplyCommands :: (Op -> Int -> Int) -> [Command] -> Int
sumApplyCommands f = sum
                   . map snd
                   . M.toList
                   . foldl' (applyCommandHM f) M.empty

solve1 :: String -> Int
solve1 = sumApplyCommands apply1 . parseOrDie commands


solve2 :: String -> Int
solve2 = sumApplyCommands apply2 . parseOrDie commands
