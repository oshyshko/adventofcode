module Y15.TestD21 where

import           Test.Hspec

import           Y15.D21

spec :: Spec
spec = do
    it "combs" $ do
        combs @Int 0 []     `shouldBe` [[]]
        combs @Int 1 []     `shouldBe` []
        combs @Int 0 [1..3] `shouldBe` [[]]
        combs @Int 1 [1..3] `shouldBe` [[1], [2], [3]]
        combs @Int 2 [1..3] `shouldBe` [[1,2], [1,3], [2,3]]

    it "playerDefeatsBoss" $ do
        playerDefeatsBoss (Character 8 5 5) (Character 12 7 2) `shouldBe` True
        playerDefeatsBoss (Character 8 5 5) (Character 13 7 2) `shouldBe` False
