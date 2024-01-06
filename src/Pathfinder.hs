module Pathfinder where

import qualified Data.HashPSQ                as QH
import qualified Data.IntMap.Strict          as MI
import qualified Data.IntPSQ                 as QI
import qualified Data.Map.Strict             as M
import qualified Data.STRef                  as S
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           Util

-- p  - point
-- s  - score
-- ss - score storage

data World p s where
    World :: (Bounded s, Ord s, Num s) =>
        { neighbors :: p -> [p]
        , cost      :: p -> p -> s
        } -> World p s


class (Ord p, Ord s, Bounded s) => Queue q p s where
    qSingleton :: p -> q
    qAlterMin  :: q -> (Maybe (p,s,()), q)
    qInsert    :: p -> s -> q -> q

instance (p ~ Int, Bounded s, Ord s) => Queue (QI.IntPSQ s ()) p s where
    qSingleton p = QI.singleton p maxBound ()
    qAlterMin    = QI.alterMin (,Nothing)
    qInsert p s  = QI.insert p s ()

instance (Hashable p, Ord p, Bounded s, Ord s) => Queue (QH.HashPSQ p s ()) p s where
    qSingleton p = QH.singleton p maxBound ()
    qAlterMin    = QH.alterMin (,Nothing)
    qInsert p s  = QH.insert p s ()


class ScoreStorage ss p s where
    sEmpty :: ss
    sGet   :: p -> ss -> s
    sSet   :: p -> s -> ss -> ss

instance (Ord p, Bounded s) => ScoreStorage (Map p s) p s where
    sEmpty = M.empty
    sGet   = M.findWithDefault maxBound
    sSet   = M.insert

instance (p ~ Int, Bounded s) => ScoreStorage (IntMap s) p s where
    sEmpty = MI.empty
    sGet   = MI.findWithDefault maxBound
    sSet   = MI.insert


{-# INLINE[1] minScoreMap #-}                                           -- keep to reduce heap allocations
minScoreMap :: forall p s. (Ord p, Hashable p, Bounded s, Ord s) => World p s -> p -> p -> Maybe s
minScoreMap = minScoreMap' @(QH.HashPSQ p s ()) @(Map p s)

{-# INLINE[1] minScoreIntMap #-}                                         -- keep to reduce heap allocations
minScoreIntMap :: forall p s. (p ~ Int, Bounded s, Ord s) => World p s -> p -> p -> Maybe s
minScoreIntMap = minScoreMap' @(QI.IntPSQ s ()) @(IntMap s)

{-# INLINE[1] minScoreMap' #-}                                           -- keep to reduce heap allocations
minScoreMap'
    :: forall q ss p s. (Queue q p s, ScoreStorage ss p s)
    => World p s -> p -> p -> Maybe s
minScoreMap' World{neighbors,cost} start goal =
    fix2
        (qSingleton @q @p @s start)                                     -- open
        (sSet @ss @p @s start 0 (sEmpty @ss @p @s))                     -- xy2score
        \loop open ss ->
            qAlterMin @q @p @s open & \case
                (Nothing, _) -> Nothing                                 -- exhausted?
                (Just (current, _, _), openEjected) ->
                    let currentScore = sGet current ss
                    in if current == goal                               -- goal reached?
                        then Just currentScore
                        else
                              neighbors current                         -- for each neighbor
                            & foldl'                                    -- apply collected changes
                                (\(openAcc,ssAcc) np ->
                                    let nScore    = cost current np  + currentScore
                                        nScoreOld = sGet np ss
                                    in if nScore < nScoreOld
                                        then ( qInsert np nScore openAcc
                                             , sSet np nScore ssAcc
                                             )
                                        else (openAcc,ssAcc)
                                    )
                                (openEjected,ss)
                            & uncurry loop

{-# INLINE[1] minScoreMVector #-}                                       -- keep to reduce heap allocations
minScoreMVector
    :: forall q p s. (p ~ Int, VUM.Unbox s, q ~ QI.IntPSQ s ())
    => Int -> World p s -> p -> p -> Maybe s
minScoreMVector scoreStorageSize World{neighbors,cost} start goal =
    runST $ do
        let mkStoreStorage    = VUM.replicate scoreStorageSize maxBound
            readScore         = flip $ VUM.read @(ST _)
            writeScore p s ss = VUM.write ss p s

        openR <- S.newSTRef $ qSingleton @q @p @s start
        ss <- mkStoreStorage
        writeScore start 0 ss

        fix \loop -> do
            S.readSTRef openR <&> qAlterMin @q @p @s >>= \case
                (Nothing, _) -> pure Nothing                            -- exhausted?
                (Just (current, _, _), openEjected) -> do
                    currentScore <- readScore current ss
                    if current == goal                                  -- goal reached?
                        then pure $ Just currentScore
                        else do
                            S.writeSTRef openR openEjected
                            forM_ (neighbors current) \nXyi -> do       -- for each neighbor
                                let nScore = cost current nXyi + currentScore
                                nScoreOld <- readScore nXyi ss
                                when (nScore < nScoreOld) $ do          -- apply changes
                                    S.modifySTRef' openR (qInsert nXyi nScore)
                                    writeScore nXyi nScore ss
                            loop
