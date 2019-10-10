module Y15.D06Shared where

import           Control.Monad                 (forM_)
import           Data.Array.MArray             (MArray, readArray, writeArray)
import           Data.Foldable                 (foldl')
import           Data.Functor                  (($>))
import           Data.Ix                       (range)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe)
import           Text.ParserCombinators.Parsec (Parser, char, digit, endBy,
                                                many, space, string, try, (<|>))

import           Util

side :: Int -- TODO determine sides from input?
side = 1000

type XY         = (Int, Int)
type Brightness = Int
data Op         = On | Off | Toggle deriving Show
type Command    = (Op, XY, XY)

-- turn off 199,133 through 461,193
-- toggle 322,558 through 977,958
-- turn on 226,196 through 599,390
commands :: Parser [Command]
commands = command `endBy` eol
  where
    command :: Parser Command
    command = (,,) <$> op <* space
                   <*> xy <* string " through "
                   <*> xy

    op :: Parser Op
    op =    try (string "turn on")  $> On
        <|> try (string "turn off") $> Off
        <|>      string "toggle"    $> Toggle

    xy :: Parser XY
    xy = (,) <$> (read <$> many digit)
             <* char ','
             <*> (read <$> many digit)

{-# INLINE apply1 #-}
apply1 :: Op -> Brightness -> Brightness
apply1 op v = case op of
    On     -> 1
    Off    -> 0
    Toggle -> if v == 1 then 0 else 1

{-# INLINE apply2 #-}
apply2 :: Op -> Brightness -> Brightness
apply2 op v = case op of
    On     -> v + 1
    Off    -> max 0 (v -1)
    Toggle -> v + 2

{-# INLINE applyCommandArray #-}
applyCommandArray :: MArray a e m => (Op -> e -> e) -> a XY e -> Command -> m ()
applyCommandArray f a (op, xy0, xy1) =
    forM_ (range (xy0, xy1))
        (\xy -> do
            v <- readArray a xy
            writeArray a xy (f op v))

-- TODO combine with applyCommandArray?
{-# INLINE applyCommandMap #-}
applyCommandMap :: (Op -> Brightness -> Brightness) -> M.Map XY Brightness -> Command -> M.Map XY Brightness
applyCommandMap f mm (op, (x0,y0), (x1,y1)) =
    foldl' (\m xy -> M.insert xy (f op (fromMaybe 0 $ M.lookup xy m)) m)
           mm
           [ (x,y) | x <- [x0..x1],
                     y <- [y0..y1] ]
