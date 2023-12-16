module Y23.D15 where

import qualified Data.Map.Strict as M
import           Imports
import           Parser

type BoxId = Int
data Labeled a = Labeled { label :: String , value :: a } deriving Show
data Op = Add Int | Remove deriving Show

ops :: Parser [Labeled Op]
ops =
    op `sepBy` char ','
  where
    op = Labeled
        <$> many1 letter
        <*> (   (Add    <$> (char '=' *> (read <$> count 1 digit)))
            <|> (Remove <$   char '-'))

-- rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
hash :: String -> Int
hash =
    foldl' go 0
  where
    go h x = (h + ord x) * 17 `rem` 256

solve1 :: String -> Int
solve1 = sum . fmap hash . splitOn "," . head . lines

solve2 :: String -> Int
solve2 s =
      parseOrDie ops s
    & foldl' addOrRemove M.empty
    & M.toList
    & concatMap (\(boxId, labeledFls) ->
        [ (boxId + 1) * slotId * fl
            | (slotId, Labeled _ fl) <- zip [1..] labeledFls ] )
    & sum
  where
    addOrRemove :: Map BoxId [Labeled Int] -> Labeled Op -> Map BoxId  [Labeled Int]
    addOrRemove m = \case
        Labeled l (Add n) -> M.alter  (add $ Labeled l n)  (hash l) m
        Labeled l Remove  -> M.update (remove l) (hash l) m

    add :: Labeled Int -> Maybe [Labeled Int] -> Maybe [Labeled Int]
    add ln = Just . \case
        Nothing -> [ln]                                     -- create new
        Just xs ->                                          -- update existing
            case findIndex ((label ln ==) . label) xs of
                Just i ->                                   -- ... update
                    let (as,zs) = splitAt i xs
                    in as ++ [ln] ++ tail zs
                Nothing -> xs ++ [ln]                       -- ... append

    remove :: String -> [Labeled Int] -> Maybe [Labeled Int]
    remove l = Just . filter ((/= l) . label)
