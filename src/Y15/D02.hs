module Y15.D02 where

import           Geom.XYZ
import           Parser

-- 29x13x26
-- 11x11x14
whls :: Parser [XYZ]
whls =
    whl `endBy` eol
  where
    whl = XYZ <$> natural <* char 'x' <*> natural <* char 'x' <*> natural

solve1 :: String -> Int
solve1 =
      sum
    . map (\(XYZ w h l) ->
           let a = w * h
               b = h * l
               c = w * l
           in (a + b + c) * 2 + minimum [a, b, c])
    . parseOrDie whls

solve2 :: String -> Int
solve2 =
      sum
    . map (\(XYZ w h l) -> (w*h*l) + 2 * minimum [w+h, h+l, w+l])
    . parseOrDie whls
