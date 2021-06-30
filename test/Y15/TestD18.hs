module Y15.TestD18 where

import qualified Data.Vector.Unboxed.Mutable as VUM
import           Test.Hspec

import           Util
import           Imports
import           Y15.D18

spec :: Spec
spec = do
    let x      = True
        o      = False

        state0 = [ [o, x, o, x, o, x]
                 , [o, o, o, x, x, o]
                 , [x, o, o, o, o, x]
                 , [o, o, x, o, o, o]
                 , [x, o, x, o, o, x]
                 , [x, x, x, x, o, o] ]

        states = parseOrDie lights . unlines <$>
                 [ [ ".#.#.#" -- initial state
                   , "...##."
                   , "#....#"
                   , "..#..."
                   , "#.#..#"
                   , "####.." ]

                 , [ "..##.." -- after 1 step
                   , "..##.#"
                   , "...##."
                   , "......"
                   , "#....."
                   , "#.##.." ]

                 , [ "..###." -- after 2 steps
                   , "......"
                   , "..###."
                   , "......"
                   , ".#...."
                   , ".#...." ]

                 , [ "...#.." -- after 3 steps
                   , "......"
                   , "...#.."
                   , "..##.."
                   , "......"
                   , "......" ]

                 , [ "......" -- after 4 steps
                   , "......"
                   , "..##.."
                   , "..##.."
                   , "......"
                   , "......" ] ]

        states2 = parseOrDie lights . unlines <$>
                 [ [ "##.#.#" -- initial state
                   , "...##."
                   , "#....#"
                   , "..#..."
                   , "#.#..#"
                   , "####.#" ]

                 , [ "#.##.#" -- after 1 step
                   , "####.#"
                   , "...##."
                   , "......"
                   , "#...#."
                   , "#.####" ]

                 , [ "#..#.#" -- after 2 steps
                   , "#....#"
                   , ".#.##."
                   , "...##."
                   , ".#..##"
                   , "##.###" ]

                 , [ "#...##" -- after 3 steps
                   , "####.#"
                   , "..##.#"
                   , "......"
                   , "##...."
                   , "####.#" ]

                 , [ "#.####" -- after 4 steps
                   , "#....#"
                   , "...#.."
                   , ".##..."
                   , "#....."
                   , "#.#..#" ]

                 , [ "##.###" -- after 5 steps
                   , ".##..#"
                   , ".##..."
                   , ".##..."
                   , "#.#..."
                   , "##...#" ] ]

    it "parseOrDie lights" $
        head states `shouldBe` state0

    it "mkLights + unLinghts" $
        mkLights state0
            >>= unLights
            >>= (`shouldBe` state0)

    it "getOr" $ do
        b <- mkLights state0
        let yz = pred . length        $ state0
            xz = pred . length . head $ state0

        sequence (getOr undefined b
            <$> [YX yy xx | yy <- [0..yz] , xx <- [0..xz]])
                >>= (`shouldBe` join state0)

        getOr True  b (YX (yz+1) (xz+1)) >>= (`shouldBe` True)
        getOr False b (YX (yz+1) (xz+1)) >>= (`shouldBe` False)

    it "neighboursOnAround" $ do
        b <- mkLights [ [x, x, x]
                           , [o, x, o]
                           , [o, x, o] ]

        sequence (neighborsOnAround b
            <$> [YX yy xx | yy <- [-1..3], xx <- [-1..3]])
                >>= (`shouldBe` join
                    [ [1, 2, 3, 2, 1]
                    , [1, 2, 3, 2, 1]
                    , [1, 4, 4, 4, 1]
                    , [0, 2, 1, 2, 0]
                    , [0, 1, 1, 1, 0] ])

    it "fate1" $ do
        (tick1 undefined undefined True  <$> [0..8]) `shouldBe` [o, o, x, x, o, o, o, o, o]
        (tick1 undefined undefined False <$> [0..8]) `shouldBe` [o, o, o, x, o, o, o, o, o]

    it "tick" $ do
        src <- mkLights
            [ [x, x, x]
            , [o, x, o]
            , [o, x, o] ]

        dstVector <- VUM.replicate (VUM.length (vector src)) False

        let dst = src{vector=dstVector}

        tickLights tick1 src dst

        let e = [ [x, x, x]
                , [o, o, o]
                , [o, o, o] ]

        unLights dst >>= (`shouldBe` e)

    it "tickTimes fate1" $
        forM_ (zip [0..] states) $ \(i, e) -> do
            a <- mkLights . head $ states
            tickTimes tick1 a i
            unLights a >>= \aa -> (i,aa) `shouldBe` (i, e)

    it "tickTimes fate2" $
        forM_ (zip [0..] states2) $ \(i, e) -> do
            a <- mkLights . head $ states2
            tickTimes tick2 a i
            unLights a >>= \aa -> (i,aa) `shouldBe` (i, e)
