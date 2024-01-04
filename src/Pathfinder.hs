module Pathfinder where

import qualified Data.IntMap.Strict          as M
import qualified Data.IntPSQ                 as Q
import qualified Data.STRef                  as S
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           Util

-- p - point
-- v - value
-- s - score

{-# INLINE[1] minScoreIntMap #-} -- TODO remove?
minScoreIntMap :: (p ~ Int, Bounded s, Ord s, Num s)
    => (p -> [p]) -> (p -> v) -> (v -> s)                               -- board
    -> p -> p -> Maybe s                                                -- start goal
minScoreIntMap = minScore M.empty (M.findWithDefault maxBound) M.insert

{-# INLINE[1] minScore #-} -- TODO remove?
minScore :: (p ~ Int, Bounded s, Ord s, Num s)
    => ss -> (p -> ss -> s) -> (p -> s -> ss -> ss)                     -- score storage
    -> (p -> [p]) -> (p -> v) -> (v -> s)                               -- board
    -> p -> p -> Maybe s                                                -- start goal
minScore ssEmpty getScore setScore neighbors at score start goal =
    fix2
        (Q.singleton start maxBound ())                                 -- open
        (setScore start 0 ssEmpty)                                      -- xy2score
        \loop open ss ->
            Q.alterMin (,Nothing) open & \case
                (Nothing, _) -> Nothing                                 -- exhausted ?
                (Just (current, _, _), openEjected) ->
                    let currentScore = getScore current ss
                    in if current == goal                               -- goal reached?
                        then Just currentScore
                        else
                              neighbors current                         -- for each neighbor
                            & foldl'                                    -- apply collected changes
                                (\(openAcc,ssAcc) nXyi ->
                                    let nRisk     = at nXyi
                                        nScore    = score nRisk  + currentScore
                                        nScoreOld = getScore nXyi ss
                                    in if nScore < nScoreOld
                                        then ( Q.insert nXyi nScore () openAcc
                                             , setScore nXyi nScore ssAcc
                                             )
                                        else (openAcc,ssAcc)
                                    )
                                (openEjected,ss)
                            & uncurry loop

{-# INLINE[1] minScoreMVector #-}
minScoreMVector :: (p ~ Int, Bounded s, Ord s, Num s, VUM.Unbox s)
    => Int                                                              -- score storage (size)
    -> (p -> [p]) -> (p -> v) -> (v -> s)                               -- board
    -> p -> p -> Maybe s                                                -- start goal
minScoreMVector scoreStorageSize neighbors at score start goal =
    runST $ do
        let mkStoreStorage    = VUM.replicate scoreStorageSize maxBound
            readScore         = flip $ VUM.read @(ST _)
            writeScore p s ss = VUM.write ss p s

        open <- S.newSTRef $ Q.singleton start maxBound ()
        ss <- mkStoreStorage
        writeScore start 0 ss

        fix \loop -> do
            S.readSTRef open <&> Q.alterMin (,Nothing) >>= \case
                (Nothing, _) -> pure Nothing                            -- exhausted ?
                (Just (current, _, _), openEjected) -> do
                    currentScore <- readScore current ss
                    if current == goal                                  -- goal reached?
                        then pure $ Just currentScore
                        else do
                            S.writeSTRef open openEjected
                            forM_ (neighbors current) \nXyi -> do       -- for each neighbor
                                let nRisk  = at nXyi
                                    nScore = score nRisk + currentScore
                                nScoreOld <- readScore nXyi ss
                                when (nScore < nScoreOld) $ do          -- apply changes
                                    S.modifySTRef' open (Q.insert nXyi nScore ())
                                    writeScore nXyi nScore ss
                            loop
