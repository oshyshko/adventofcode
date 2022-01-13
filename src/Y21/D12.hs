module Y21.D12 where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Imports
import           Util

type Cave = String

start, end :: Cave
start = "start"
end   = "end"

small :: Cave -> Bool
small = isLower . head

-- start-A
-- start-b
-- A-c
-- A-b
-- b-d
-- A-end
-- b-end
--
parse :: String -> Map Cave [Cave]
parse =
    M.fromListWith (<>) . expand . parseOrDie links
  where
      expand = (=<<) \(k,v) -> [(k,[v]), (v, [k])]
      links  = link `endBy` eol
      link   = (,) <$> many letter <* string "-" <*> many letter

solve1 :: String -> Int
solve1 s =
    length $ paths (parse s) S.empty start
  where
    paths :: Map Cave [Cave] -> Set Cave -> Cave -> [[Cave]]
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
    paths :: Map Cave [Cave] -> Bool -> Set Cave -> Cave -> [[Cave]]
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
