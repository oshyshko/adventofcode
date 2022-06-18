module Y21.TestD20 where

import           Data.String.QQ
import           Test.Hspec

import           Parser
import           Y21.D20

spec :: Spec
spec = do
    let (r,i0) = parseOrDie replacementAndImage repAndIter0
        i1     = parseOrDie image iter1
        i2     = parseOrDie image iter2

    -- it "bits2int" $ do
    --     bits2int     [0] `shouldBe` 0
    --     bits2int     [1] `shouldBe` 1
    --     bits2int   [1,0] `shouldBe` 2
    --     bits2int [0,1,1] `shouldBe` 3
    --     bits2int [1,0,0] `shouldBe` 4

    --     bits2int [1,1,1, 1,1,1, 1,1,1] `shouldBe` 511

    it "enhance" $ do
        enhance r i0 `shouldBe` i1
        enhance r i1 `shouldBe` i2

    it "countSetLights" $ do
        countSetLights            i0  `shouldBe` 10
        countSetLights (enhance r i0) `shouldBe` 24

    it "solve1+2" $ do
        solve1 repAndIter0 `shouldBe` 35
        -- NOTE commented, too slow
        -- solve2 repAndIter0 `shouldBe` 3351

    it "getReplacementIndex" $ do
        getReplacementIndex 0 i0   2  `shouldBe` 34     -- 000 100 010


repAndIter0 :: String
repAndIter0 = [s|
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
|]

iter1 :: String
iter1 = [s|
.##.##.
#..#.#.
##.#..#
####..#
.#..##.
..##..#
...#.#.
|]

iter2 :: String
iter2 = [s|
.......#.
.#..#.#..
#.#...###
#...##.#.
#.....#.#
.#.#####.
..#.#####
...##.##.
....###..
|]
