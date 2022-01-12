module Y21.D12 where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Imports

-- start-A
-- start-b
-- A-c
-- A-b
-- b-d
-- A-end
-- b-end
--
parse :: String -> Map String [String]
parse =
      M.fromListWith (<>)
    . (=<<) (\[k,v] -> [(k,[v]), (v, [k])])
    . fmap (splitOn "-")
    . lines

start, end :: String
start = "start"
end   = "end"

small :: String -> Bool
small = isLower . head

solve1 :: String -> Int
solve1 s =
    length $ paths (parse s) S.empty start
  where
    paths :: Map String [String] -> Set String -> String -> [[String]]
    paths m v x
        | x == end       = [[end]]
        | x `S.member` v = []
        | otherwise =
            let newV = if small x then S.insert x v else v
            in m M.! x >>= (fmap (x :) . paths m newV)

solve2 :: String -> Int
solve2 s =
    length $ paths (parse s) False  S.empty start
  where
    paths :: Map String [String] -> Bool -> Set String -> String -> [[String]]
    paths m visitedTwice v x
        | x == end  = [[end]]
        | otherwise =
            let xInV = x `S.member` v
            in if xInV && visitedTwice
                then []
                else
                    let newV = if small x then S.insert x v else v
                    in (m M.! x) & filter (/= start)
                        >>= (fmap (x :) . paths m (xInV || visitedTwice) newV)
