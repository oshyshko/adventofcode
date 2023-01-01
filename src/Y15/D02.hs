module Y15.D02 where

import           Geom.XYZ
import           Imports

-- 29x13x26
-- 11x11x14
parseWHL :: String -> [XYZ]
parseWHL =
    map whl . lines
  where
    whl = (\[w, h, l] -> XYZ w h l) . map read . splitOn "x"

solve1 :: String -> Int
solve1 =
      sum
    . map (\(XYZ w h l) ->
           let a = w * h
               b = h * l
               c = w * l
           in (a + b + c) * 2 + minimum [a, b, c])
    . parseWHL

solve2 :: String -> Int
solve2 =
      sum
    . map (\(XYZ w h l) -> (w*h*l) + 2 * minimum [w+h, h+l, w+l])
    . parseWHL
