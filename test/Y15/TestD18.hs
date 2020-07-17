module Y15.TestD18 where

import           Control.Monad     (forM_, join)
import           Data.Array.IO     (IOUArray)
import           Data.Array.MArray (getBounds, newArray)
import           Test.Hspec

import           Util
import           Y15.D18

spec :: SpecWith ()
spec = describe "Y15.D18" $ do
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

    it "mkLights" $
        (mkLights state0 :: IO (IOUArray YX Bool))
            >>= unLights
            >>= (`shouldBe` state0)

    it "getOr" $ do
        m <- mkLights state0 :: IO (IOUArray YX Bool)
        yxaz <- getBounds m
        let yz = pred . length        $ state0
            xz = pred . length . head $ state0

        sequence (getOr undefined m yxaz
            <$> [YX yy xx | yy <- [0..yz] , xx <- [0..xz]])
                >>= (`shouldBe` join state0)

        getOr True  m yxaz (YX (yz+1) (xz+1)) >>= (`shouldBe` True)
        getOr False m yxaz (YX (yz+1) (xz+1)) >>= (`shouldBe` False)

    it "neighboursOnAround" $ do
        m <- mkLights [ [x, x, x]
                           , [o, x, o]
                           , [o, x, o] ] :: IO (IOUArray YX Bool)

        yxaz <- getBounds m

        sequence (neighborsOnAround m yxaz
            <$> [YX yy xx | yy <- [-1..3], xx <- [-1..3]])
                >>= (`shouldBe` join [ [1, 2, 3, 2, 1]
                                     , [1, 2, 3, 2, 1]
                                     , [1, 4, 4, 4, 1]
                                     , [0, 2, 1, 2, 0]
                                     , [0, 1, 1, 1, 0] ])

    it "fate1" $ do
        (tick1 undefined undefined True  <$> [0..8]) `shouldBe` [o, o, x, x, o, o, o, o, o]
        (tick1 undefined undefined False <$> [0..8]) `shouldBe` [o, o, o, x, o, o, o, o, o]

    it "tick" $ do
        src <- mkLights [ [x, x, x]
                             , [o, x, o]
                             , [o, x, o] ] :: IO (IOUArray YX Bool)
        yxaz <- getBounds src
        dst <- newArray yxaz False :: IO (IOUArray YX Bool)

        tickLights tick1 yxaz src dst

        a <- unLights dst
        e <- (mkLights [ [x, x, x]
                            , [o, o, o]
                            , [o, o, o] ]  :: IO (IOUArray YX Bool)) >>= unLights

        a `shouldBe` e

    it "tickTimes fate1" $
        forM_ (zip [0..] states) $ \(i, es) -> do
            a <- mkLights . head $ states :: IO (IOUArray YX Bool)
            tickTimes tick1 a i
            unLights a >>= \as -> (i,as) `shouldBe` (i,es)

    it "tickTimes fate2" $
        forM_ (zip [0..] states2) $ \(i, es) -> do
            a <- mkLights . head $ states2 :: IO (IOUArray YX Bool)
            tickTimes tick2 a i
            unLights a >>= \as -> (i,as) `shouldBe` (i,es)
