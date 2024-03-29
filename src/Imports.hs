module Imports
    ( module Combinatorics
    , module Control.Applicative
    , module Control.Arrow
    , module Control.Exception
    , module Control.Exception.Safe
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Primitive
    , module Control.Monad.ST
    , module Data.Bifunctor
    , module Data.Bit
    , module Data.Bool
    , module Data.Char
    , module Data.Either
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Functor.Identity
    , module Data.HashMap.Strict
    , module Data.Hashable
    , module Data.Int
    , module Data.IntMap.Strict
    , module Data.List
    , module Data.List.Split
    , module Data.Map.Strict
    , module Data.Maybe
    , module Data.Set
    , module Data.STRef
    , module Data.Tuple.Strict
    , module Data.Word
    , module GHC.Generics
    , module System.IO
    , module Text.Printf
    ) where

import           Combinatorics           (tuples)
import           Control.Applicative     (liftA2, (<|>))
import           Control.Arrow           ((&&&))
import           Control.Exception       (evaluate)
import           Control.Exception.Safe  (SomeException, bracket, catch,
                                          finally)
import           Control.Monad           (forM, forM_, guard, join, replicateM,
                                          unless, when, (<=<), (>=>))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Primitive (PrimMonad (..))
import           Control.Monad.ST        (ST, fixST, runST)
import           Data.Bifunctor          (bimap, first, second)
import           Data.Bit                (Bit (..), unBit)
import           Data.Bool               (bool)
import           Data.Char               (chr, digitToInt, intToDigit,
                                          isAlphaNum, isAsciiLower,
                                          isAsciiUpper, isDigit, isLower,
                                          isSpace, isUpper, ord, toLower,
                                          toUpper)
import           Data.Either             (partitionEithers)
import           Data.Foldable           (find, foldl', foldlM, maximumBy,
                                          minimumBy)
import           Data.Function           (fix, on, (&))
import           Data.Functor            (void, ($>), (<&>))
import           Data.Functor.Identity   (Identity (..))
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import           Data.Int                (Int16, Int32, Int64, Int8)
import           Data.IntMap.Strict      (IntMap)
import           Data.List               (delete, dropWhileEnd, elemIndex,
                                          findIndex, foldl1', group,
                                          intercalate, intersperse, isInfixOf,
                                          isPrefixOf, isSuffixOf, nub,
                                          partition, permutations, sort, sortBy,
                                          sortOn, tails, transpose)
import           Data.List.Split         (chunksOf, divvy, splitOn, splitWhen)
import           Data.Map.Strict         (Map)
import           Data.Maybe              (catMaybes, fromJust, fromMaybe,
                                          isJust, isNothing, listToMaybe,
                                          mapMaybe, maybeToList)
import           Data.Set                (Set)
import           Data.STRef              (STRef, modifySTRef, modifySTRef',
                                          newSTRef, readSTRef, writeSTRef)
import           Data.Tuple.Strict       (T1 (..), T2 (..), T3 (..), T4 (..),
                                          T5 (..), T6 (..), T7 (..), T8 (..),
                                          scurry, sfst, ssnd, sswap, suncurry)
import           Data.Word               (Word16, Word32, Word64, Word8)
import           GHC.Generics            (Generic)
import           System.IO               (hFlush, stdout)
import           Text.Printf             (printf)
