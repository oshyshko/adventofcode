module Y23.D05 where

import           Imports
import           Parser
import           Util

data Range     = Range { offset :: Int, size :: Int } deriving Show
data Movable a = Movable Int a                        deriving Show

data Setup = Setup
    { seeds  :: [Int]
    , stages :: [[Movable Range]]       -- [[steps-in-stages]]
    } deriving Show

-- seeds: 79 14 55 13
--
-- seed-to-soil map:
-- 50 98 2
-- 52 50 48
--
-- soil-to-fertilizer map:
-- 0 15 37
-- 37 52 2
-- 39 0 15
setup :: Parser Setup
setup = Setup
    <$> (string "seeds:" *> many1 (padded natural) <* eol <* eol)
    <*> (stage `sepBy` eol)
  where
    stage :: Parser [Movable Range]
    stage =
           (many1 letter *> string "-to-" *> many1 letter *> string " map:" *> eol)
        *> (toFromSize `endBy` eol)
    toFromSize =
        Movable
            <$> padded natural
            <*> (Range <$> padded natural <*> padded natural)

solve1 :: String -> Int
solve1 s =
    let Setup{seeds,stages} = parseOrDie setup s
    in    seeds
        & fmap (\seed -> foldl' fallthroughStage seed stages)
        & minimum
  where
    fallthroughStage :: Int -> [Movable Range] -> Int
    fallthroughStage x [] = x
    fallthroughStage x ((Movable to (Range{offset,size})):ms) =
        if offset <= x && x <= offset + size
            then x + to - offset
            else fallthroughStage x ms

data Marked a = Marked Bool a deriving Show

solve2 :: String -> Int
solve2 s =
    let Setup{seeds,stages} = parseOrDie setup s
    in    seeds                                                 -- [Int]
        & fmap (uncurry Range) . divvy2 2                       -- [Range]
        & concatMap (\r -> foldl' fallthroughStage [r] stages)  -- [Range]
        & fmap offset                                           -- [Int]
        & minimum
  where
    fallthroughStage :: [Range] -> [Movable Range] -> [Range]
    fallthroughStage rs ms =
          ms
        & foldl' addStagePiece (Marked False <$> rs)
        & fmap (\(Marked _ x) -> x)

    addStagePiece :: [Marked Range] -> Movable Range -> [Marked Range]
    addStagePiece xs m =
        xs & concatMap (\x@(Marked marked r) ->
            if marked
                then [x]
                else applyStepInStage r m)

    applyStepInStage :: Range -> Movable Range -> [Marked Range]
    applyStepInStage r (Movable to m) =
        -- a --------- b   a --------- b         a --------- b
        --    c --- d               c --- d   c --- d
        let a = offset r; b = offset r + size r
            c = offset m; d = offset m + size m
        in    [a, b, c, d]
            & sort
            & divvy2 1
            & filter (\(o1,_) -> a <= o1 && o1 < b) -- within `r`?
            & concatMap \(o1, o2) ->
                if c <= o1 && o1 <= d               -- within `m`?
                    then [Marked True  $ Range (o1 - offset m + to) (o2 - o1)]
                    else [Marked False $ Range o1 (o2 - o1)]
