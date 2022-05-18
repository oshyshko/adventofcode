module Y21.TestD15 where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import           Test.Hspec

import           Y21.D15

spec :: Spec
spec = do
    it "findBack" $ do
        let cameFrom :: IntMap (XYI, Score) = M.fromList
                [ (1, (0,0))
                , (2, (0,0))
                , (3, (1,0))
                , (4, (3,0))
                , (5, (2,0))
                ]

        findWayBack cameFrom 0 `shouldBe` [0]
        findWayBack cameFrom 1 `shouldBe` [0,1]
        findWayBack cameFrom 2 `shouldBe` [0,2]
        findWayBack cameFrom 3 `shouldBe` [0,1,3]
        findWayBack cameFrom 4 `shouldBe` [0,1,3,4]
        findWayBack cameFrom 5 `shouldBe` [0,2,5]
        findWayBack cameFrom 6 `shouldBe` [6]
