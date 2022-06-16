module Y21.D08 where

import           Imports
import           Parser

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

type Pattern = String
type PSet    = Set Char

data Entry = Entry
    { patterns :: [Pattern]
    , output   :: [Pattern]
    } deriving Show

-- acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
entries :: Parser [Entry]
entries =
    entry `endBy` eol
  where
    entry      = Entry <$> segments 10 <* padded (char '|') <*> segments 4
    segments n = count n (sort <$> many1 (oneOf "abcdefg") <* pad)

decodeOutput :: Entry -> Int
decodeOutput Entry{patterns,output} =
    let x235 = psetsOfLen 5
        x069 = psetsOfLen 6

        b   = freqToSegment 6   -- b:0___456_89
        e   = freqToSegment 4   -- e:0_2___6_8_
        f   = freqToSegment 9   -- f:01_3456789

        x1  = one $ psetsOfLen 2                                    -- 1
        x5  = filterOne (\x -> b `isIn` x)                    x235  -- 5

        z = [ filterOne (\x -> (e `isIn` x) && (x1 `isIn` x)) x069  -- 0
            , x1                                                    -- 1
            , filterOne (f `isNotIn`)                         x235  -- 2
            , filterOne (\x -> b `isNotIn` x && f `isIn` x)   x235  -- 3
            , one $ psetsOfLen 4                                    -- 4
            , x5                                                    -- 5
            , filterOne (\x -> e `isIn` x && x5 `isIn` x)     x069  -- 6
            , one $ psetsOfLen 3                                    -- 7
            , one $ psetsOfLen 7                                    -- 8
            , filterOne (\x -> e `isNotIn` x)                 x069  -- 9
            ]

        patternToDigit = M.fromList $ zip (S.toList <$> z) [0..]

    in sum . zipWith (*) [1000,100,10,1] $ fmap (patternToDigit M.!) output
  where
    one :: Show a => [a] -> a
    one = \case [x] -> x; x -> error $ show x

    isIn        =         S.isSubsetOf
    isNotIn s x = not (s `S.isSubsetOf` x)

    filterOne p xs = one $ filter p xs

    psetsOfLen n = S.fromList <$> filter ((== n). length) patterns

    freqToSegment n =
          S.fromList
        . one
        . map fst
        . filter ((== n) . snd)
        . map (\x -> (x, length x))
        . group
        . sort
        . concat $ patterns

solve1 :: String -> Int
solve1 =
      length
    . concatMap (filter ((`S.member` lengths1478) . length) . output)
    . parseOrDie entries
  where
    lengths1478 = S.fromList [2, 4, 3, 7]

solve2 :: String -> Int
solve2 =
      sum
    . map decodeOutput
    . parseOrDie entries
