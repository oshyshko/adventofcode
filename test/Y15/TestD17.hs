module Y15.TestD17 where

import           Test.Hspec

import           Y15.D17

spec :: Spec
spec = do
    it "parseContainers" $
        parseContainers "1\n2\n3\n" `shouldBe` [1, 2, 3]

    it "configs 25 [20, 15, 10, 5, 5]" $
        configs 25 [20, 15, 10, 5, 5] `shouldBe`
            [ [20, 5]
            , [20, 5]
            , [15, 10]
            , [15, 5, 5]
            ]
