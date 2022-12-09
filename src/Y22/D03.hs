module Y22.D03 where

import qualified Data.Set as S
import           Imports

priority :: Char -> Int
priority x
    | 'a' <= x && x <= 'z' =  1 + fromEnum x - fromEnum 'a'
    | 'A' <= x && x <= 'Z' = 27 + fromEnum x - fromEnum 'A'
    | otherwise = error $ "Unexpected item: " ++ show x

intersection :: [String] -> Char
intersection = head . S.toList . foldl1' S.intersection . fmap S.fromList -- TODO assert single

solve1 :: String -> Int
solve1 =
      sum
    . fmap (\s -> priority . intersection . chunksOf (length s `quot` 2) $ s)
    . lines

solve2 :: String -> Int
solve2 =
      sum
    . fmap (priority . intersection)
    . divvy 3 3
    . lines

