module TestUtil where

import           Test.Hspec

import           Util

spec :: Spec
spec = do
    it "divvy2" $ do
        divvy2 @Int 0 []     `shouldBe` []
        divvy2 @Int 1 []     `shouldBe` []
        divvy2 @Int 1 [1..3] `shouldBe` [(1,2), (2,3)]
        divvy2 @Int 2 [1..3] `shouldBe` [(1,2)]

    it "tuples2" $ do
        tuples2 @Int []     `shouldBe` []
        tuples2 @Int [1..3] `shouldBe` [(1,2), (1,3), (2,3)]

    it "size2humanSize" $
        size2humanSize . fst <$> sizeHumanSizes
            `shouldBe` snd <$> sizeHumanSizes
          where
            sizeHumanSizes =
              [ (         0,   "0B")
              , (         1,   "1B")
              , (       125, "125B")
              , (       999, "999B")
              , (      1000,   "1.0K")
              , (      1023,   "1.0K")
              , (      1024,   "1.0K")
              , (      1025,   "1.0K")
              , (      2048,   "2.0K")
              , (      8192,   "8.0K")
              , (     16384,  "16.0K")
              , (     32768,  "32.0K")
              , (     65536,  "64.0K")
              , (    131072, "128.0K")
              , (    262144, "256.0K")
              , (    524288, "512.0K")
              , (   1048575,   "1.0M")
              , (   1048576,   "1.0M")
              , (   2097152,   "2.0M")
              , (   4194304,   "4.0M")
              , (1073741823,   "1.0G")
              , (1073741824,   "1.0G")
              , (1073741825,   "1.0G")
              ]
