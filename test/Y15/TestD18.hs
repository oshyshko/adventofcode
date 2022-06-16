module Y15.TestD18 where

import qualified Data.Vector.Unboxed.Mutable as VUM
import           Test.Hspec

import           Imports
import           MVec2 (MVec2(..))
import           Parser
import           XY
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

        let h = length state0
            w = length . head $ state0

        wh b `shouldBe` XY w h

        sequence
            [ getOr undefined b (XY xx yy)
            | yy <- [0..h-1]
            , xx <- [0..w-1]
            ]
                >>= (`shouldBe` join state0)

        getOr True  b (XY w h) >>= (`shouldBe` True)
        getOr False b (XY w h) >>= (`shouldBe` False)

    it "neighboursOnAround" $ do
        b <- mkLights [ [x, x, x]
                      , [o, x, o]
                      , [o, x, o] ]

        sequence
            [neighborsOnAround b (XY xx yy)
            | yy <- [-1..3]
            , xx <- [-1..3]
            ]
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

        dstVec <- VUM.replicate (VUM.length (vec src)) False

        let dst = src{vec=dstVec}

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
