module Y15.TestD16 where

import           Test.Hspec

import           Util
import           Y15.D16

spec :: Spec
spec = do
    let input = "Sue 1: goldfish: 6, trees:    9, akitas: 0\n"
             ++ "Sue 2: goldfish: 7, trees:    1, akitas: 0\n"
             ++ "Sue 3: cars:     2, perfumes: 5, cats:   2\n"

    it "parseOrDie sue2clues" $
        parseOrDie sue2clues input `shouldBe`
            [ (1, [(Goldfish, 6), (Trees,    9), (Akitas, 0)])
            , (2, [(Goldfish, 7), (Trees,    1), (Akitas, 0)])
            , (3, [(Cars,     2), (Perfumes, 5), (Cats,   2)])]
