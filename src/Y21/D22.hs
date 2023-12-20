module Y21.D22 where

import           Geom.Range   (Range (..), intersection, size)
import           Geom.Spatial (Tree, mkTree, set, toList)
import           Geom.XYZ     (XYZ (..), xyzFold)
import           Imports
import           Parser

-- on x=10..12,y=10..12,z=-12..-10
-- off x=9..11,y=9..11,z=9..11
rebootSteps :: Parser [(Range XYZ Int, Bool)]
rebootSteps =
    record `endBy` eol
  where
    record :: Parser (Range XYZ Int, Bool)
    record = do
        v <- try (string "on" $> True) <|> (string "off" $> False)
        x <- string " x=" *> integer; a <- string ".." *> integer
        y <- string ",y=" *> integer; b <- string ".." *> integer
        z <- string ",z=" *> integer; c <- string ".." *> integer
        -- convert (offset,offset) to (offset,size)
        pure (Range (XYZ x y z) (XYZ (a-x+1) (b-y+1) (c-z+1)), v)

volume :: Tree (Range XYZ Int) XYZ Bool -> Int
volume = sum . fmap (xyzFold (*) . size . fst) . filter snd . toList @(Range XYZ Int)

solve :: ([(Range XYZ Int, Bool)] -> [(Range XYZ Int, Bool)]) -> String -> Int
solve f =
      volume
    . foldl' (\t (s,v) -> set v s t) (mkTree False)
    . f
    . parseOrDie rebootSteps

solve1, solve2 :: String -> Int
solve1 = solve $ mapMaybe \(s,v) -> (,v) <$> intersection @XYZ @Int s (Range (-50) 101)
solve2 = solve id
