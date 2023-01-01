module Y21.D22 where

import           Geom.Box     (Box (..), intersection)
import           Geom.Spatial
import           Geom.XYZ
import           Imports
import           Parser

-- on x=10..12,y=10..12,z=-12..-10
-- off x=9..11,y=9..11,z=9..11
rebootSteps :: Parser [(Bool, Box XYZ Int)]
rebootSteps =
    record `endBy` eol
  where
    record :: Parser (Bool, Box XYZ Int)
    record = do
        v  <- try (string "on" $> True) <|> (string "off" $> False)
        x <- string " x=" *> integer
        a <- string ".."  *> integer
        y <- string ",y=" *> integer
        b <- string ".."  *> integer
        z <- string ",z=" *> integer
        c <- string ".."  *> integer
        -- convert (offset,offset) to (offset,size
        pure (v, Box (XYZ x y z) (XYZ (a-x+1) (b-y+1) (c-z+1)))

volume :: Tree XYZ Int Bool -> Int
volume = sum . fmap (\(v,Box _ whd) -> if v then xyzFold (*) whd else 0) . toList

solve1, solve2 :: String -> Int
solve1 =
      volume
    . foldl' (\t (v,s) -> set v s t) (mkTree False)
    . mapMaybe (\(v,s) -> (v,) <$> intersection @XYZ @Int s (Box (-50) 101))
    . parseOrDie rebootSteps

solve2 =
      volume
    . foldl' (\t (v,s) -> set v s t) (mkTree False)
    . parseOrDie rebootSteps
