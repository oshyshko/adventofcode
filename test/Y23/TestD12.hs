{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y23.TestD12 where

import           Test.Hspec

import           Y23.D12

spec :: Spec
spec = do
    it "startsWith" $ do
        -- "#." -- head 1
        startsWith ""    1 False `shouldBe` False   -- 0
        startsWith "."   1 False `shouldBe` False   -- 1
        startsWith "#"   1 False `shouldBe` False
        startsWith "?"   1 False `shouldBe` False
        startsWith ".."  1 False `shouldBe` False   -- 2
        startsWith ".#"  1 False `shouldBe` False
        startsWith ".?"  1 False `shouldBe` False
        startsWith "#."  1 False `shouldBe` True
        startsWith "#?"  1 False `shouldBe` True
        startsWith "##"  1 False `shouldBe` False
        startsWith "?."  1 False `shouldBe` True
        startsWith "?#"  1 False `shouldBe` False
        startsWith "??"  1 False `shouldBe` True

        -- "##" -- terminal 2
        startsWith ""    2 True `shouldBe` False   -- 0
        startsWith "."   2 True `shouldBe` False   -- 1
        startsWith "#"   2 True `shouldBe` False
        startsWith "?"   2 True `shouldBe` False
        startsWith ".."  2 True `shouldBe` False   -- 2
        startsWith ".#"  2 True `shouldBe` False
        startsWith ".?"  2 True `shouldBe` False
        startsWith "#."  2 True `shouldBe` False
        startsWith "#?"  2 True `shouldBe` True
        startsWith "##"  2 True `shouldBe` True
        startsWith "?."  2 True `shouldBe` False
        startsWith "?#"  2 True `shouldBe` True
        startsWith "??"  2 True `shouldBe` True

        -- TODO check remaining pattern for #s when ns=[]
