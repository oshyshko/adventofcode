module Day01 where

char2value :: Char -> Int
char2value x = case x of '(' ->  1
                         ')' -> -1
                         _   ->  0

solve1 :: String -> Int
solve1 = sum . map char2value

solve2 :: String -> Int
solve2 xs = fst . head
  $ dropWhile (\(_, x) -> x >= 0)
  $ zip [1..]
  $ scanl (+) 0
  $ map char2value xs

main :: IO ()
main = do
  s <- readFile "Day01.txt"
  print $ solve1 s
  print $ solve2 s
