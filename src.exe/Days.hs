{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wunused-imports #-}
module Days where

import qualified Y15.D01
import qualified Y15.D02
import qualified Y15.D03
import qualified Y15.D04
import qualified Y15.D05
import qualified Y15.D06
import qualified Y15.D07
import qualified Y15.D08
import qualified Y15.D09
import qualified Y15.D10
import qualified Y15.D11
import qualified Y15.D12
import qualified Y15.D13
import qualified Y15.D14
import qualified Y15.D15
import qualified Y15.D16
import qualified Y15.D17
import qualified Y15.D18
import qualified Y15.D19
import qualified Y15.D20
import qualified Y15.D21

import qualified Y21.D01
import qualified Y21.D02
import qualified Y21.D03
import qualified Y21.D04
import qualified Y21.D05
import qualified Y21.D06

import qualified Data.Map.Strict as M

import qualified DaysTH
import           Types (DayPrefix, Day(..))

moduleName2day :: M.Map DayPrefix Day
moduleName2day =
    M.fromList $ (\d@Day{dayPrefix} -> (dayPrefix, d)) <$> $(DaysTH.days)
