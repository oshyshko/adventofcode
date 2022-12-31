module Y21.TestD22 where

import           Test.Hspec

import           Box
import           Imports
import           Spatial
import           XYZ
import           Y21.D22

t, f :: Tree XYZ Int Bool
t = Leaf True
f = Leaf False

b :: XYZ -> XYZ -> Box XYZ Int
b (XYZ x y z) (XYZ w h d) =
    Box
        (XYZ (cor x w) (cor y h) (cor z d))
        (XYZ (abs w)   (abs h)   (abs d))
  where
    cor xx ww = if ww < 0 then xx + ww else xx

spec :: Spec
spec = do
    it "volume" $ do
        volume  f                                                   `shouldBe`  0
        volume (f & set True (b 0 1))                               `shouldBe`  1
        volume (f & set True (b 0 2))                               `shouldBe`  8
        volume (f & set True (b 0 3))                               `shouldBe` 27
        volume (f & set True (b 0 4))                               `shouldBe` 64

        volume (f & set True (b 0 5)       & set True (b 1 2))      `shouldBe`  125 -- 5^3
        volume (f & set True (b (-10) 20)  & set True (b 0 1))      `shouldBe` 8000 -- 20^3
        volume (f & set True (b 0 1)       & set True (b (-10) 20)) `shouldBe` 8000 -- 20^3
