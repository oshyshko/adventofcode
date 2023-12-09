{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y23.D08 where

import qualified Data.Map.Strict          as M

import           Imports
import           Parser
import           Util

data Turn = L | R deriving Show

data Instructions = Instructions
    { turns   :: [Turn]
    , network :: [(String, (String, String))]
    } deriving Show

instructions :: Parser Instructions
instructions =
    Instructions
        <$> many1 turn <* eol <* eol
        <*> n2nn `endBy` eol
  where
    turn = (L <$ char 'L') <|> (R <$ char 'R')
    node = count 3 alphaNum
    n2nn = (,)
        <$> (node <* padded (char '='))
        <*> ((,)
            <$> (char '(' *> padded node <* char ',')
            <*> (padded node <* char ')'))

stepsToEnd :: String -> (String -> Bool) -> Instructions -> Int
stepsToEnd start isEnd Instructions{turns,network} =
    let node2nextLR = M.fromList network
    in fix2 start (zip [1..] $ cycle turns)
        \loop node ((i,turn):ts) ->
            let (l,r) = node2nextLR M.! node
                nextNode = case turn of
                    L -> l
                    R -> r
            in if isEnd nextNode
                then i
                else loop nextNode ts

solve1 :: String -> Int
solve1 = stepsToEnd "AAA" (== "ZZZ") . parseOrDie instructions

solve2 :: String -> Int
solve2 s =
    let ins@Instructions{network} = parseOrDie instructions s
    in    fmap fst network
        & filter ("A" `isSuffixOf`)                                 -- all starts
        & fmap (\start -> stepsToEnd start ("Z" `isSuffixOf`) ins)
        & foldl' lcm 1                                              -- where all converge
