module TestSpatial where

import           Test.Hspec

import           Box
import           Imports
import           Spatial
import           XYZ

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
    it "bestSplitCenter" $ do
        bestSplitCenter (b 0 9) (b 0 1) `shouldBe` 1    -- 012345678
                                                        -- 0^
        bestSplitCenter (b 0 9) (b 0 4) `shouldBe` 4    -- 012345678
                                                        -- 0123^
        bestSplitCenter (b 0 9) (b 2 2) `shouldBe` 4    -- 012345678
                                                        --   23^
        bestSplitCenter (b 0 9) (b 2 3) `shouldBe` 5    -- 012345678
                                                        --   234^
        bestSplitCenter (b 0 9) (b 2 4) `shouldBe` 2    -- 012345678
                                                        --   ^345
        bestSplitCenter (b 0 9) (b 2 5) `shouldBe` 2    -- 012345678
                                                        --   ^3456
        bestSplitCenter (b 0 9) (b 2 6) `shouldBe` 2    -- 012345678
                                                        --   ^34567

    it "split" $ do
        split (b 10 9) 15 `shouldBe`
            [ b (XYZ 10 10 10) (XYZ 5 5 5)
            , b (XYZ 10 10 15) (XYZ 5 5 4)
            , b (XYZ 10 15 10) (XYZ 5 4 5)
            , b (XYZ 10 15 15) (XYZ 5 4 4)
            , b (XYZ 15 10 10) (XYZ 4 5 5)
            , b (XYZ 15 10 15) (XYZ 4 5 4)
            , b (XYZ 15 15 10) (XYZ 4 4 5)
            , b (XYZ 15 15 15) (XYZ 4 4 4)
            ]

    it "set" $ do
        (t & set True  (b 0 1)) `shouldBe` t
        (f & set False (b 0 1)) `shouldBe` f
        (f & set False (b 0 2)) `shouldBe` f

        (f & set True  (b 0 1)) `shouldBe` Split 0 [f, f, f, f, f, f, f, Split 1 [t, f, f, f, f, f, f, f]]
        (f & set True  (b 0 4)) `shouldBe` Split 0 [f, f, f, f, f, f, f, Split 4 [t, f, f, f, f, f, f, f]]

        (f & set True  (b 0 1)
           & set True  (b (-9) 9)) `shouldBe`
            Split 0 [Split (-9) [f, f, f, f, f, f, f, t], f, f, f, f, f, f, Split 1 [t, f, f, f, f, f, f, f]]

    it "toList" $ do
        let lpm = fmap snd . filter fst . toList

        lpm f `shouldBe` []
        lpm (f & set True (b 0 1))                                  `shouldBe` [b 0 1]
        lpm (f & set True (b 0 2))                                  `shouldBe` [b 0 2]
        lpm (f & set True (b 0 3))                                  `shouldBe` [b 0 3]
        lpm (f & set True (b 0 1) & set True (b 1 1))               `shouldBe` [b 0 1, b 1 1]
        lpm (f & set True (b 0 1) & set True (b 4 5))               `shouldBe` [b 0 1, b 4 5]
        lpm (f & set True (b (-10) 1))                              `shouldBe` [b (-10) 1]
