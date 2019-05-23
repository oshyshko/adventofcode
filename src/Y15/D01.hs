{-# LANGUAGE LambdaCase #-}

module Y15.D01 where

char2move :: Char -> Int
char2move = \case
    '(' ->  1
    ')' -> -1
    x   ->  error $ "Unexpected character: " ++ [x]

solve1 :: String -> Int
solve1 = sum . map char2move

solve2 :: String -> Int
solve2 = fst
       . head
       . dropWhile (\(_, x) -> x >= 0)
       . zip [0..]
       . scanl (+) 0
       . map char2move
