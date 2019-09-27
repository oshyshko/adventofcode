module Main where

import           Test.Hspec

import           Util
import qualified Y15.D15

main :: IO ()
main = hspec $
  describe "Y15.D15" $
    it "score TODO" $ do
        let input = "Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5\n"
                 ++ "PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1\n"
                 ++ "Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6\n"
                 ++ "Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8\n"
            ingrs = parseOrDie Y15.D15.ingredients input

        Y15.D15.score ingrs [1,2,3,4] `shouldBe` (-192)
