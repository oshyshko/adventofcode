module Geom.TestRange where

import           Test.Hspec

import           Geom.Point
import           Geom.Range
import           Geom.XY
import           Geom.XYZ
import           Imports

r :: XYZ -> XYZ -> Range XYZ Int
r (XYZ x y z) (XYZ w h d) =
    Range
        (XYZ (cor x w) (cor y h) (cor z d))
        (XYZ (abs w)   (abs h)   (abs d))
  where
    cor xx ww = if ww < 0 then xx + ww else xx

ii :: Point p c => Range p c -> Range p c -> Maybe (Range p c) -> IO ()
ii q p e = do
        let qp  = q `intersects` p
            pq  = p `intersects` q
            qp' = q `intersection` p
            pq' = p `intersection` q
        qp  `shouldBe` pq
        qp' `shouldBe` pq'
        isJust qp' `shouldBe` qp
        qp' `shouldBe` e

spec :: Spec
spec = do
    it "intersects + intersection" $ do
        ii (r 0 1) (r 2 1) Nothing
        ii (r 0 1) (r 1 1) Nothing

        -- completely inside
        ii (r 1 2)      (r 0 5) $ Just (r 1 2)
        ii (r 0 5)      (r 1 2) $ Just (r 1 2)
        ii (r (-10) 20) (r 0 5) $ Just (r 0 5)

        -- cross (no corners of one range withing another)
        ii (Range @XY @Int (XY 0 5) (XY 10 5)) (Range (XY 5 0) (XY 5 10)) $ Just (Range 5 5)

        -- 8 corners
        ii (r 1 2) (r 2 (XYZ   2    2    2 )) $ Just (r 2 (XYZ   1    1    1 ))
        ii (r 1 2) (r 2 (XYZ   2    2  (-2))) $ Just (r 2 (XYZ   1    1  (-1)))
        ii (r 1 2) (r 2 (XYZ   2  (-2)   2 )) $ Just (r 2 (XYZ   1  (-1)   1 ))
        ii (r 1 2) (r 2 (XYZ   2  (-2) (-2))) $ Just (r 2 (XYZ   1  (-1) (-1)))
        ii (r 1 2) (r 2 (XYZ (-2)   2    2 )) $ Just (r 2 (XYZ (-1)   1    1 ))
        ii (r 1 2) (r 2 (XYZ (-2)   2  (-2))) $ Just (r 2 (XYZ (-1)   1  (-1)))
        ii (r 1 2) (r 2 (XYZ (-2) (-2)   2 )) $ Just (r 2 (XYZ (-1) (-1)   1 ))
        ii (r 1 2) (r 2 (XYZ (-2) (-2) (-2))) $ Just (r 2 (XYZ (-1) (-1) (-1)))
