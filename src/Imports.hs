module Imports
    ( module Control.Applicative
    , module Control.Arrow
    , module Control.Exception
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Primitive
    , module Data.Bifunctor
    , module Data.Bool
    , module Data.Char
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Functor.Identity
    , module Data.HashMap.Strict
    , module Data.Int
    , module Data.Hashable
    , module Data.IntMap.Strict
    , module Data.List
    , module Data.List.Split
    , module Data.Map.Strict
    , module Data.Maybe
    , module Data.Set
    , module Data.Tuple.Strict
    , module Data.Word
    , module System.IO
    , module Text.Printf
    , module Text.Read
    ) where

import           Control.Applicative     (liftA2, (<|>))
import           Control.Arrow           ((&&&))
import           Control.Exception       (SomeException, catch, evaluate)
import           Control.Monad           (forM, forM_, guard, join, replicateM,
                                          unless, when, (<=<), (>=>))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.Bifunctor          (bimap, first, second)
import           Data.Bool               (bool)
import           Data.Char               (digitToInt, intToDigit, isAlphaNum,
                                          isAsciiLower, isLower, isSpace,
                                          isUpper, toLower, toUpper)
import           Data.Foldable           (foldl', foldlM, maximumBy, minimumBy)
import           Data.Function           (fix, on, (&))
import           Data.Functor            (void, ($>), (<&>))
import           Data.Functor.Identity   (Identity (..))
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import           Data.Int                (Int16, Int32, Int64, Int8)
import           Data.IntMap.Strict      (IntMap)
import           Data.List               (delete, dropWhileEnd, findIndex,
                                          foldl1', group, intercalate,
                                          isInfixOf, isPrefixOf, isSuffixOf,
                                          nub, partition, permutations, sort,
                                          sortBy, sortOn, transpose)
import           Data.List.Split         (chunksOf, divvy, splitOn, splitWhen)
import           Data.Map.Strict         (Map)
import           Data.Maybe              (catMaybes, fromJust, fromMaybe,
                                          listToMaybe, mapMaybe, maybeToList)
import           Data.Set                (Set)
import           Data.Tuple.Strict       (T1 (..), T2 (..), T3 (..), T4 (..),
                                          T5 (..), T6 (..), T7 (..), T8 (..),
                                          scurry, sfst, ssnd, sswap, suncurry)
import           Data.Word               (Word16, Word32, Word8)
import           System.IO               (hFlush, stdout)
import           Text.Printf             (printf)
import           Text.Read               (readMaybe)


