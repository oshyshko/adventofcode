module Letters
    ( parse
    ) where

-- letter parsing for Y21.D13 and Y22.D10

import qualified Data.Map.Strict as M
import           Imports

parse :: [String] -> String
parse =
        fmap ((pat2char M.!) . (concat . take 4))
    . divvy 4 5
    . transpose
  where
    pat2char :: Map String Char
    pat2char =
        M.fromList $ zip
            (fmap concat $ divvy 4 5 $ transpose $ drop 1 rawChars)
            (fmap head $ divvy 1 5 $ head rawChars)
    rawChars :: [String]
    rawChars =
        [ "A    B    C    E    F    G    H    J    K    L    P    R    U    Z   "
        , " ##  ###   ##  #### ####  ##  #  #   ## #  # #    ###  ###  #  # ####"
        , "#  # #  # #  # #    #    #  # #  #    # # #  #    #  # #  # #  #    #"
        , "#  # ###  #    ###  ###  #    ####    # ##   #    #  # #  # #  #   # "
        , "#### #  # #    #    #    # ## #  #    # # #  #    ###  ###  #  #  #  "
        , "#  # #  # #  # #    #    #  # #  # #  # # #  #    #    # #  #  # #   "
        , "#  # ###   ##  #### #     ### #  #  ##  #  # #### #    #  #  ##  ####"
        ]
