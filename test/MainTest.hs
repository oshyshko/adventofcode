module Main where

import           Test.Hspec

import qualified TestUtil
import qualified Y15.TestD15
import qualified Y15.TestD16
import qualified Y15.TestD17
import qualified Y15.TestD18
import qualified Y15.TestD19
import qualified Y15.TestD21

import qualified Y21.TestD16
import qualified Y21.TestD18

main :: IO ()
main = hspec $ do
    context "Util"    TestUtil.spec

    context "Y15.D15" Y15.TestD15.spec
    context "Y15.D16" Y15.TestD16.spec
    context "Y15.D17" Y15.TestD17.spec
    context "Y15.D18" Y15.TestD18.spec
    context "Y15.D19" Y15.TestD19.spec
    context "Y15.D21" Y15.TestD21.spec

    context "Y21.D16" Y21.TestD16.spec
    context "Y21.D18" Y21.TestD18.spec
