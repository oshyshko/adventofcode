module TestBox where

import           Test.Hspec

import           Box
import           XYZ

b :: XYZ -> XYZ -> Box XYZ Int
b (XYZ x y z) (XYZ w h d) =
    Box
        (XYZ (cor x w) (cor y h) (cor z d))
        (XYZ (abs w)   (abs h)   (abs d))
  where
    cor xx ww = if ww < 0 then xx + ww else xx

spec :: Spec
spec = do
    it "intersection" $ do
        b 0 1           `intersection` b 2 1 `shouldBe` Nothing
        b 0 1           `intersection` b 1 1 `shouldBe` Nothing

        b 1 2           `intersection` b 0 5 `shouldBe` Just (b 1 2)
        b 0 5           `intersection` b 1 2 `shouldBe` Just (b 1 2)
        b (-100) 200    `intersection` b 0 4 `shouldBe` Just (b 0 4)

        -- 8 corners
        b 1 2 `intersection` b 2 (XYZ   2    2    2 ) `shouldBe` Just (b 2 (XYZ   1    1    1 ))
        b 1 2 `intersection` b 2 (XYZ   2    2  (-2)) `shouldBe` Just (b 2 (XYZ   1    1  (-1)))
        b 1 2 `intersection` b 2 (XYZ   2  (-2)   2 ) `shouldBe` Just (b 2 (XYZ   1  (-1)   1 ))
        b 1 2 `intersection` b 2 (XYZ   2  (-2) (-2)) `shouldBe` Just (b 2 (XYZ   1  (-1) (-1)))
        b 1 2 `intersection` b 2 (XYZ (-2)   2    2 ) `shouldBe` Just (b 2 (XYZ (-1)   1    1 ))
        b 1 2 `intersection` b 2 (XYZ (-2)   2  (-2)) `shouldBe` Just (b 2 (XYZ (-1)   1  (-1)))
        b 1 2 `intersection` b 2 (XYZ (-2) (-2)   2 ) `shouldBe` Just (b 2 (XYZ (-1) (-1)   1 ))
        b 1 2 `intersection` b 2 (XYZ (-2) (-2) (-2)) `shouldBe` Just (b 2 (XYZ (-1) (-1) (-1)))
