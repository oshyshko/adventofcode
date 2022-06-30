module Y21.TestD22 where

import           Test.Hspec

import           Imports
import           XYZ
import           Y21.D22

t, f :: Octo Bool
t = Leaf True
f = Leaf False

l :: XYZ -> XYZ -> Line
l (XYZ x y z) (XYZ w h d) =
    Line
        (XYZ (cor x w) (cor y h) (cor z d))
        (XYZ (abs w)   (abs h)   (abs d))
  where
    cor xx ww = if ww < 0 then xx + ww else xx

spec :: Spec
spec = do
    it "intersect" $ do
        l 0 1           `intersect` l 2 1 `shouldBe` Nothing
        l 0 1           `intersect` l 1 1 `shouldBe` Nothing

        l 1 2           `intersect` l 0 5 `shouldBe` Just (l 1 2)
        l 0 5           `intersect` l 1 2 `shouldBe` Just (l 1 2)
        l (-100) 200    `intersect` l 0 4 `shouldBe` Just (l 0 4)

        -- 8 corners
        l 1 2 `intersect` l 2 (XYZ   2    2    2 ) `shouldBe` Just (l 2 (XYZ   1    1    1 ))
        l 1 2 `intersect` l 2 (XYZ   2    2  (-2)) `shouldBe` Just (l 2 (XYZ   1    1  (-1)))
        l 1 2 `intersect` l 2 (XYZ   2  (-2)   2 ) `shouldBe` Just (l 2 (XYZ   1  (-1)   1 ))
        l 1 2 `intersect` l 2 (XYZ   2  (-2) (-2)) `shouldBe` Just (l 2 (XYZ   1  (-1) (-1)))
        l 1 2 `intersect` l 2 (XYZ (-2)   2    2 ) `shouldBe` Just (l 2 (XYZ (-1)   1    1 ))
        l 1 2 `intersect` l 2 (XYZ (-2)   2  (-2)) `shouldBe` Just (l 2 (XYZ (-1)   1  (-1)))
        l 1 2 `intersect` l 2 (XYZ (-2) (-2)   2 ) `shouldBe` Just (l 2 (XYZ (-1) (-1)   1 ))
        l 1 2 `intersect` l 2 (XYZ (-2) (-2) (-2)) `shouldBe` Just (l 2 (XYZ (-1) (-1) (-1)))


    it "bestSplitCenter" $ do
        bestSplitCenter (l 0 9) (l 0 1) `shouldBe` 1    -- 012345678
                                                        -- 0^
        bestSplitCenter (l 0 9) (l 0 4) `shouldBe` 4    -- 012345678
                                                        -- 0123^
        bestSplitCenter (l 0 9) (l 2 2) `shouldBe` 4    -- 012345678
                                                        --   23^
        bestSplitCenter (l 0 9) (l 2 3) `shouldBe` 5    -- 012345678
                                                        --   234^
        bestSplitCenter (l 0 9) (l 2 4) `shouldBe` 2    -- 012345678
                                                        --   ^345
        bestSplitCenter (l 0 9) (l 2 5) `shouldBe` 2    -- 012345678
                                                        --   ^3456
        bestSplitCenter (l 0 9) (l 2 6) `shouldBe` 2    -- 012345678
                                                        --   ^34567

    it "split" $ do
        split (l 10 9) 15 `shouldBe`
            [ l (XYZ 10 10 10) (XYZ 5 5 5)
            , l (XYZ 10 10 15) (XYZ 5 5 4)
            , l (XYZ 10 15 10) (XYZ 5 4 5)
            , l (XYZ 10 15 15) (XYZ 5 4 4)
            , l (XYZ 15 10 10) (XYZ 4 5 5)
            , l (XYZ 15 10 15) (XYZ 4 5 4)
            , l (XYZ 15 15 10) (XYZ 4 4 5)
            , l (XYZ 15 15 15) (XYZ 4 4 4)
            ]

    it "set" $ do
        (t & set t (l 0 1)) `shouldBe` t
        (f & set f (l 0 1)) `shouldBe` f
        (f & set f (l 0 2)) `shouldBe` f

        (f & set t (l 0 1)) `shouldBe` Split 0 [f, f, f, f, f, f, f, Split 1 [t, f, f, f, f, f, f, f]]
        (f & set t (l 0 4)) `shouldBe` Split 0 [f, f, f, f, f, f, f, Split 4 [t, f, f, f, f, f, f, f]]

        (f & set t (l 0 1) & set t (l (-9) 9)) `shouldBe`
            Split 0 [Split (-9) [f, f, f, f, f, f, f, t], f, f, f, f, f, f, Split 1 [t, f, f, f, f, f, f, f]]

    it "list" $ do
        let lpm = fmap snd . filter fst . list

        lpm f `shouldBe` []
        lpm (f & set t (l 0 1))                 `shouldBe` [l 0 1]
        lpm (f & set t (l 0 2))                 `shouldBe` [l 0 2]
        lpm (f & set t (l 0 3))                 `shouldBe` [l 0 3]
        lpm (f & set t (l 0 1) & set t (l 1 1)) `shouldBe` [l 0 1, l 1 1]
        lpm (f & set t (l 0 1) & set t (l 4 5)) `shouldBe` [l 0 1, l 4 5]
        lpm (f & set t (l (-10) 1))             `shouldBe` [l (-10) 1]

    it "volume" $ do
        volume  f                   `shouldBe`  0
        volume (f & set t (l 0 1))  `shouldBe`  1
        volume (f & set t (l 0 2))  `shouldBe`  8
        volume (f & set t (l 0 3))  `shouldBe` 27
        volume (f & set t (l 0 4))  `shouldBe` 64

        volume (f & set t (l 0 5)       & set t (l 1 2))        `shouldBe`  125 -- 5^3
        volume (f & set t (l (-10) 20)  & set t (l 0 1))        `shouldBe` 8000 -- 20^3
        volume (f & set t (l 0 1)       & set t (l (-10) 20))   `shouldBe` 8000 -- 20^3
