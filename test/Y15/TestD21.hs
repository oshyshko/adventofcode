module Y15.TestD21 where

import           Test.Hspec

import           Y15.D21

spec :: Spec
spec =
    it "playerDefeatsBoss" $ do
        playerDefeatsBoss (Character 8 5 5) (Character 12 7 2) `shouldBe` True
        playerDefeatsBoss (Character 8 5 5) (Character 13 7 2) `shouldBe` False
