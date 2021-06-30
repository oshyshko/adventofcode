module Y15.TestD19 where

import qualified Data.Map.Strict as M
import           Test.Hspec

import           Imports
import           Y15.D19

spec :: Spec
spec = do
    it "replaceDisticntCount" $ do
        singleReplacements (M.fromList
            [ ("H", ["HO", "OH"])
            , ("O", ["HH"])
            ])
            "HOH"
          `shouldBe` ["HOOH","OHOH","HHHH","HOHO","HOOH"]

        singleReplacements (M.fromList
            [ ("H", ["HO", "OH"])
            , ("O", ["HH"])
            ])
            "H2O"
          `shouldBe` ["HO2O","OH2O","H2HH"]

        (length . nub . singleReplacements (M.fromList
            [( "H", ["HO", "OH"])
            , ("O", ["HH"])
            ]))
            "HOHOHO"
          `shouldBe` 7
