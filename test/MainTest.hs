module Main where

import           Test.Hspec

import qualified Y15.TestD15
import qualified Y15.TestD16

main :: IO ()
main = hspec $ do
    Y15.TestD15.spec
    Y15.TestD16.spec
