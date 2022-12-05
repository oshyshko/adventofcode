module Y21.D10 where

import qualified Data.Map.Strict as M

import           Imports

opener2closer :: Map Char Char
opener2closer = M.fromList $ zip "([{<" ")]}>"

reduce :: String -> String -> (String, String)              -- stack -> input -> (stack, input)
reduce stack [] = (stack, [])                               -- stop     (exhausted input)
reduce stack (i:input)
    | i `M.member` opener2closer = reduce (i:stack) input   -- continue (push opener)
    | otherwise = case stack of
        [] -> (stack, i:input)                              -- stop     (unexpected closer)
        (x:xs)
            | opener2closer M.! x == i -> reduce xs input   -- continue (pop opener)
            | otherwise -> (stack, i:input)                 -- stop     (closer doesn't match opener)

solve1 :: String -> Int
solve1 =
      sum
    . fmap (score . reduce "")
    . lines
  where
    score (_, [])  = 0
    score (_, i:_) = closer2score M.! i
    closer2score = M.fromList $ zip ")]}>" [3, 57, 1197, 25137]

solve2 :: String -> Int
solve2 =
      middle
    . sort
    . filter (/= 0)
    . fmap (score . reduce "")
    . lines
  where
    score (xs, []) = foldl' (\s x -> 5 * s + opener2score M.! x) 0 xs
    score _        = 0
    opener2score = M.fromList $ zip "([{<" [1, 2, 3, 4]
    middle xs = length xs & \l ->
        if even l
            then error "even length"
            else xs !! (l `quot` 2)
