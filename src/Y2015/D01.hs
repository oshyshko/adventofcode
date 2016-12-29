module Y2015.D01 where

char2move :: Char -> Int
char2move x = case x of '(' ->  1
                        ')' -> -1
                        _   ->  error $ "Unexpected character: " ++ [x]

solve1 :: String -> Int
solve1 = sum . map char2move

solve2 :: String -> Int
solve2 = fst
       . head
       . dropWhile (\(_, x) -> x >= 0)
       . zip [1..]
       . scanl (+) 0
       . map char2move

solve :: String -> [Int]
solve = sequence [solve1, solve2]
