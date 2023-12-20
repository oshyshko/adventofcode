module Geom.TestSpatial where

import           Test.Hspec

import           Geom.Range
import           Geom.Spatial
import           Geom.XYZ
import           Imports

t, f :: Tree (Range XYZ Int) XYZ Bool
t = Leaf True
f = Leaf False

r :: XYZ -> XYZ -> Range XYZ Int
r (XYZ x y z) (XYZ w h d) =
    Range
        (XYZ (cor x w) (cor y h) (cor z d))
        (XYZ (abs w)   (abs h)   (abs d))
  where
    cor xx ww = if ww < 0 then xx + ww else xx

spec :: Spec
spec = do
    it "bestSplitCenter" $ do
        let bsc = bestSplitCenter @_ @XYZ

        bsc (r 0 9) (r 0 1) `shouldBe` 1    -- 012345678
                                            -- 0^
        bsc (r 0 9) (r 0 4) `shouldBe` 4    -- 012345678
                                            -- 0123^
        bsc (r 0 9) (r 2 2) `shouldBe` 4    -- 012345678
                                            --   23^
        bsc (r 0 9) (r 2 3) `shouldBe` 5    -- 012345678
                                            --   234^
        bsc (r 0 9) (r 2 4) `shouldBe` 2    -- 012345678
                                            --   ^345
        bsc (r 0 9) (r 2 5) `shouldBe` 2    -- 012345678
                                          --   ^3456
        bsc (r 0 9) (r 2 6) `shouldBe` 2    -- 012345678
                                                        --   ^34567

    it "split" $ do
        split @_ @XYZ (r 10 9) 15 `shouldBe`
            [ r (XYZ 10 10 10) (XYZ 5 5 5)
            , r (XYZ 10 10 15) (XYZ 5 5 4)
            , r (XYZ 10 15 10) (XYZ 5 4 5)
            , r (XYZ 10 15 15) (XYZ 5 4 4)
            , r (XYZ 15 10 10) (XYZ 4 5 5)
            , r (XYZ 15 10 15) (XYZ 4 5 4)
            , r (XYZ 15 15 10) (XYZ 4 4 5)
            , r (XYZ 15 15 15) (XYZ 4 4 4)
            ]

    it "set" $ do
        (t & set True  (r 0 1)) `shouldBe` t
        (f & set False (r 0 1)) `shouldBe` f
        (f & set False (r 0 2)) `shouldBe` f

        (f & set True  (r 0 1)) `shouldBe` Split 0 [f, f, f, f, f, f, f, Split 1 [t, f, f, f, f, f, f, f]]
        (f & set True  (r 0 4)) `shouldBe` Split 0 [f, f, f, f, f, f, f, Split 4 [t, f, f, f, f, f, f, f]]

        (f & set True  (r 0 1)
           & set True  (r (-9) 9)) `shouldBe`
            Split 0 [Split (-9) [f, f, f, f, f, f, f, t], f, f, f, f, f, f, Split 1 [t, f, f, f, f, f, f, f]]

    it "toList" $ do
        let lpm = fmap fst . filter snd . toList

        lpm f `shouldBe` []
        lpm (f & set True (r 0 1))                                  `shouldBe` [r 0 1]
        lpm (f & set True (r 0 2))                                  `shouldBe` [r 0 2]
        lpm (f & set True (r 0 3))                                  `shouldBe` [r 0 3]
        lpm (f & set True (r 0 1) & set True (r 1 1))               `shouldBe` [r 0 1, r 1 1]
        lpm (f & set True (r 0 1) & set True (r 4 5))               `shouldBe` [r 0 1, r 4 5]
        lpm (f & set True (r (-10) 1))                              `shouldBe` [r (-10) 1]
