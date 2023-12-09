module Y23.D08 where

import qualified Data.Map.Strict          as M

import           Imports
import           Parser

type Node = String
data Turn = L | R deriving Show

data Instructions = Instructions
    { turns       :: [Turn]
    , node2nextLR :: Map Node (Node, Node)
    } deriving Show

instructions :: Parser Instructions
instructions =
    Instructions
        <$> many1 turn <* eol <* eol
        <*> (M.fromList <$> n2nn `endBy` eol)
  where
    turn = (L <$ char 'L') <|> (R <$ char 'R')
    node = count 3 alphaNum
    n2nn = (,)
        <$> (node <* padded (char '='))
        <*> ((,)
            <$> (char '(' *> padded node <* char ',')
            <*> (padded node <* char ')'))

stepsToEnd :: Node -> (Node -> Bool) -> Instructions -> Int
stepsToEnd start isEnd Instructions{turns,node2nextLR} =
    length . takeWhile (not . isEnd) . scanl nextNode start $ cycle turns
  where
    nextNode node turn =
          node2nextLR M.! node
        & case turn of
            L -> fst
            R -> snd

solve1 :: String -> Int
solve1 = stepsToEnd "AAA" (== "ZZZ") . parseOrDie instructions

solve2 :: String -> Int
solve2 s =
    let ins@Instructions{node2nextLR} = parseOrDie instructions s
    in    M.keys node2nextLR
        & filter ("A" `isSuffixOf`)                                 -- all starts
        & fmap (\start -> stepsToEnd start ("Z" `isSuffixOf`) ins)
        & foldl' lcm 1                                              -- where all converge
