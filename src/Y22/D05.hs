module Y22.D05 where

import           Imports
import           Parser

type Crate  = Char
type Move   = (Int, Int, Int)   -- (count, from, to)

-- parser
stacksAndMoves :: Parser ([[Crate]], [Move])
stacksAndMoves =
     (,) <$> stacks
        <* eol <* eol
        <*> countFromTo `sepEndBy` eol
  where
    -- [T] [J]         [S] [J]         [N]
    -- [R] [H] [Z] [M] [T] [M] [T] [Q] [W]
    --  1   2   3   4   5   6   7   8   9
    stacks :: Parser [[Crate]]
    stacks =
           fmap catMaybes . transpose <$> many1 (try row <* eol)    -- rows
        <* many1 (padded $ natural @Int)                            -- footer
    -- [T] [J]         [S] [J]         [N]
    row :: Parser [Maybe Crate]
    row = crate `sepBy` char ' '
    -- "[C]" or "   "
    crate :: Parser (Maybe Char)
    crate =
            char '[' *> (Just <$> letter) <* char ']'
        <|> string "   " $> Nothing
    -- move 3 from 9 to 7
    countFromTo :: Parser Move
    countFromTo = (,,)
        <$ string "move"  <*> padded natural
        <* string "from " <*> padded natural
        <* string "to"    <*> padded natural

move :: ([Crate] -> [Crate]) -> [[Crate]] -> Move -> [[Crate]]
move f stacks (n, from, to) =
    let (ejected, stacks')  = adjust (splitAt n) (from - 1) stacks
        (_,       stacks'') = adjust ((undefined,) . (f ejected ++)) (to - 1) stacks'
        in stacks''
  where
    -- (old -> (ejected, new)) -> index -> values -> (ejected, new-values)
    adjust :: (a -> (a, a)) -> Int -> [a] -> (a, [a])
    adjust f i xs =
        let (l,x:r) = splitAt i xs
            (ejected, new) = f x
        in (ejected, l ++ new : r)

solve1, solve2 :: String -> String
solve1 = mapMaybe listToMaybe . uncurry (foldl' $ move reverse) . parseOrDie stacksAndMoves
solve2 = mapMaybe listToMaybe . uncurry (foldl' $ move id)      . parseOrDie stacksAndMoves
