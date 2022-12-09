module Y22.D07 where

import qualified Data.Map.Strict as M
import           Imports

type Path = [String]
data State = State { path2size :: Map Path Int, cwd :: Path }

mkPath2Size :: String -> Map Path Int
mkPath2Size =
    path2size . foldl' integrate (State M.empty []) . lines
  where
    integrate s@State {path2size, cwd} x
        | "$ ls"    `isPrefixOf` x = s
        | "dir "    `isPrefixOf` x = s
        | "$ cd /"  `isPrefixOf` x = s {cwd = []}
        | "$ cd .." `isPrefixOf` x = s {cwd = tail cwd}
        | "$ cd "   `isPrefixOf` x = s {cwd = drop (length @[] "$ cd ") x : cwd}
        | otherwise =
            s {path2size = foldl' (addSize $ readSize x) path2size (tails cwd) }
    -- ascend ["a", "b", "c"] => [["a","b","c"],["b","c"],["c"],[]]
    tails = (++ [[]]) . takeWhile (not . null) . iterate (drop 1)
    readSize = read . head . words
    addSize size p2s path = M.insertWith (+) path size p2s

solve1, solve2 :: String -> Int
solve1 = sum . filter (<= 100000) . M.elems . mkPath2Size
solve2 s =
    mkPath2Size s & \p2s ->
        let occupied    = p2s M.! []
            maxOccupied = 70000000 - 30000000           -- total -- target min free
        in fromJust . find ((<= maxOccupied) . (occupied -)) . sort . M.elems $ p2s
