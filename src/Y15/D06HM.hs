module Y15.D06HM where

import           Data.Foldable   (foldl')
import qualified Data.Map.Strict as M

import           Util
import           Y15.D06Shared

sumApplyCommands :: (Brightness -> Op -> Brightness) -> [Command] -> Brightness
sumApplyCommands f = sum
                   . map snd
                   . M.toList
                   . foldl' (applyCommandMap f) M.empty

solve1 :: String -> Int
solve1 = sumApplyCommands apply1 . parseOrDie commands

solve2 :: String -> Int
solve2 = sumApplyCommands apply2 . parseOrDie commands
