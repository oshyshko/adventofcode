module Main where

import           Control.Exception (evaluate)
import           Test.Hspec

import           Util
import           Y15.D15

main :: IO ()
main = hspec $ do
  describe "Y15.D15" $ do
    let input = "Butterscotch: capacity -1, durability -2, flavor  6, texture  3, calories 8\n"
             ++ "Cinnamon:     capacity  2, durability  3, flavor -2, texture -1, calories 3\n"
        ingrs = parseOrDie Y15.D15.ingredients input

    it "parseOrDie ingradients" $ do
        ingrs `shouldBe` --             cap dur flv txt cal
            [ Ingredient "Butterscotch" (-1)(-2)  6   3   8
            , Ingredient "Cinnamon"       2   3 (-2)(-1)  3]

    it "score" $ do
        score [] [] `shouldBe` 0

        score ingrs [44, 56] `shouldBe`
            ( (44 * (-1) + 56 *  2)     -- cap
            * (44 * (-2) + 56 *  3)     -- dur
            * (44 *   6  + 56 *(-2))    -- flv
            * (44 *   3 +  56 *(-1)) )  -- txt  ... = 62842880 (ignore cal)

        -- a recipe with a negative component should have zero score
        score [Ingredient "Cinnamon" 2 3 (-2)(-1) 3] [1] `shouldBe` 0

        evaluate (score [] [2]) `shouldThrow`
            errorCall "ingrs and quantites counts don't match: 0 vs 1"

    it "genRecipes" $ do
        genRecipes 0 0 `shouldBe` []
        genRecipes 0 1 `shouldBe` []

        genRecipes 1 0 `shouldBe` [[0]]
        genRecipes 1 1 `shouldBe` [[1]]
        genRecipes 1 2 `shouldBe` [[2]]

        genRecipes 2 0 `shouldBe` [[0,0]]
        genRecipes 2 1 `shouldBe` [[0,1],[1,0]]
        genRecipes 2 2 `shouldBe` [[0,2],[1,1],[2,0]]
        genRecipes 2 3 `shouldBe` [[0,3],[1,2],[2,1],[3,0]]

        genRecipes 3 0 `shouldBe` [[0,0,0]]
        genRecipes 3 1 `shouldBe` [[0,0,1],[0,1,0],[1,0,0]]
        genRecipes 3 2 `shouldBe` [[0,0,2],[0,1,1],[0,2,0],[1,0,1],[1,1,0],[2,0,0]]
        genRecipes 3 3 `shouldBe` [[0,0,3],[0,1,2],[0,2,1],[0,3,0],[1,0,2],[1,1,1],[1,2,0],[2,0,1],[2,1,0],[3,0,0]]
