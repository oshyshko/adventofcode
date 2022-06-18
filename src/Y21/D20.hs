module Y21.D20 where

import           Data.Bit            (Bit (..))
import           Data.Bits           ((.|.))
import qualified Data.Vector.Unboxed as VU

import           Imports
import           Parser
import           Vec2
import           XY

type Replacements = VU.Vector Bit

data Image = Image
    { background :: Bit     -- value of infinite surrounding lights
    , vec2       :: Vec2 Bit
    } deriving (Eq, Show)

-- ..#.#..#####.#.#.#.###.## ...
--
-- #..#.
-- #....
replacementAndImage :: Parser (Replacements, Image)
replacementAndImage = (,) <$> replacements <* eol <* eol <*> image

replacements :: Parser Replacements
replacements = VU.fromList <$> count 512 bit

bit :: Parser Bit
bit = (char '#' $> 1) <|> (char '.' $> 0)

image :: Parser Image
image = Image 0 . fromList <$> many1 bit `endBy` eol

getReplacementIndex :: Bit -> Image -> XY -> Int
getReplacementIndex d Image{vec2} xy =
            get 256 (XY (-1) (-1))
        .|. get 128 (XY   0  (-1))
        .|. get  64 (XY   1  (-1))
        .|. get  32 (XY (-1)   0 )
        .|. get  16 (XY   0    0 )
        .|. get   8 (XY   1    0 )
        .|. get   4 (XY (-1)   1 )
        .|. get   2 (XY   0    1 )
        .|. get   1 (XY   1    1 )
 where
    get :: Int -> XY -> Int
    get v dxy = if unBit $ getOr d vec2 (xy + dxy) then v else 0

enhance :: Replacements -> Image -> Image
enhance r im@(Image inf Vec2{wh}) =
    let newWh@(XY nw nh) = wh + 2
    in Image
        { background = r VU.! if unBit inf then 511 else 0
        , vec2 =  Vec2
            { wh = newWh
            , vec = VU.generate (nw * nh) \i ->
                r VU.! getReplacementIndex inf im (i2xy newWh i - 1)
            }
        }

countSetLights :: Image -> Int
countSetLights Image{background,vec2} =
    if unBit background
        then error "Can't count infinity of set lights"
        else VU.foldl' (\a b -> a + if unBit b then 1 else 0) 0 . vec $ vec2

solve :: Int -> String -> Int
solve n s =
    let (r,im) = parseOrDie replacementAndImage s
    in iterate (enhance r) im & (!! n) & countSetLights

solve1, solve2 :: String -> Int
solve1 = solve 2
solve2 = solve 50
