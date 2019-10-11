{-# LANGUAGE LambdaCase #-}
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
apply1 :: Brightness-> Op -> Brightness
apply1 v = \case
    On     -> 1
    Off    -> 0
    Toggle -> if v == 1 then 0 else 1

{-# INLINE apply2 #-}
apply2 :: Brightness -> Op -> Brightness
apply2 v = \case
    On     -> v + 1
    Off    -> max 0 (v -1)
    Toggle -> v + 2

{-# INLINE applyCommandArray #-}
-- NOTE using (Int, Int) vs Int as array index didn't show a noticeable time difference
applyCommandArray :: MArray a e m => (e ->  Op -> e) -> a XY e -> Command -> m ()
applyCommandArray f a (op, xy0, xy1) =
    forM_ (range (xy0, xy1))
        (\xy -> do
            v <- readArray a xy
            writeArray a xy (f v op))

-- TODO combine with applyCommandArray?
-- NOTE using (Int, Int) vs Int as map key showed 16/22s vs 11/12s difference
-- TODO consider using Data.Strict.Tuple or own ADT
{-# INLINE applyCommandMap #-}
applyCommandMap :: Num e => (e -> Op -> e) -> M.Map Int e -> Command -> M.Map Int e
applyCommandMap f mm (op, (x0,y0), (x1,y1)) =
    foldl' (\m i -> M.insert i (f (fromMaybe 0 $ M.lookup i m) op) m)
           mm
           [ x*side + y | x <- [x0..x1],
                          y <- [y0..y1] ]
