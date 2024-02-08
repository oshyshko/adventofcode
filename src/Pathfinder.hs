module Pathfinder where

import qualified Data.HashPSQ                as QH
import qualified Data.IntMap.Strict          as MI
import qualified Data.IntPSQ                 as QI
import qualified Data.Map.Strict             as M
import qualified Data.STRef                  as S
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Imports
import           Util

-- p - point
-- c - cost

-- q - queue    (open)
-- s - storage  (closed)

data Config p c where
    Config :: (Bounded c, Ord c, Num c) =>
        { neighbors :: p -> [p]
        , cost      :: p -> p -> c
        , remaining :: Maybe (p -> c)               -- estiamated cost to goal (heuristics for A*)
        , start     :: p
        , goal      :: p -> Bool
        } -> Config p c

class (Ord p, Ord c, Bounded c) => Queue q p c where
    qSingleton  :: p -> q
    qAlterMin   :: q -> (Maybe (p,c,()), q)
    qInsert     :: q -> p -> c -> q
    qToList     :: q -> [p]

class Storage s p c pc where
    sEmpty      :: s
    sGet        :: s -> pc -> p -> pc               -- s -> p-default -> p               -> (p-came-from,c)
    sSet        :: s -> p -> pc -> s                -- s -> p         -> (p-came-from,c) -> s
    sToList     :: s -> [(p,pc)]

instance (p ~ Int, Bounded c, Ord c) => Queue (QI.IntPSQ c ()) p c where
    qSingleton p    = QI.singleton p maxBound ()
    qAlterMin       = QI.alterMin (,Nothing)
    qInsert q p c   = QI.insert p c () q
    qToList         = fmap (\(p,_,_) -> p) . QI.toList

instance (Hashable p, Ord p, Bounded c, Ord c) => Queue (QH.HashPSQ p c ()) p c where
    qSingleton p    = QH.singleton p maxBound ()
    qAlterMin       = QH.alterMin (,Nothing)
    qInsert q p c   = QH.insert p c () q
    qToList         = fmap (\(p,_,_) -> p) . QH.toList


instance (Ord p) => Storage (Map p pc) p c pc where
    sEmpty      = M.empty
    sGet s d p  = M.findWithDefault d p s
    sSet s p pc = M.insert p pc s
    sToList     = M.toList


instance (p ~ Int) => Storage (IntMap pc) p c pc where
    sEmpty      = MI.empty
    sGet s d p  = MI.findWithDefault d p s
    sSet s p pc = MI.insert p pc s
    sToList     = MI.toList


sPath :: forall s p c. (Storage s p c (p,c), Eq p, Bounded c) => s -> p -> p -> [p]
sPath s start current = fix2 current [] \loop p xs ->
    let (pp, _) = sGet @s @p @c @(p,c) s (p, maxBound) p
    in (p : xs) & if start == p then id else loop pp


{-# INLINE[1] minPath #-}                                               -- keep to reduce heap allocations
minPath :: forall p c. (Ord p, Hashable p, Bounded c, Ord c, Num c) => Config p c -> Maybe (c,[p])
minPath = minPath' @(QH.HashPSQ p c ()) @(Map p (p,c))

{-# INLINE[1] minPathInt #-}                                            -- keep to reduce heap allocations
minPathInt :: forall p c. (p ~ Int, Bounded c, Ord c, Num c) => Config p c -> Maybe (c,[p])
minPathInt = minPath' @(QI.IntPSQ c ()) @(IntMap (Int,c))


{-# INLINE[1] minPath' #-}                                              -- keep to reduce heap allocations
minPath' :: forall q s p c. (Queue q p c, Storage s p c (p,c), Num c) => Config p c -> Maybe (c,[p])
minPath' config@Config{start} =
    fix2
        (qSingleton @q @p @c start)
        (sSet @s @p @c @(p,c) (sEmpty @s @p @c @(p,c)) start (start,0))
        (stepPath' Nothing config)

{-# INLINE[1] minPathTrace #-}                                          -- keep to reduce heap allocations
minPathTrace
    :: forall q s p c. (q ~ QH.HashPSQ p c (), s ~ Map p (p,c), Hashable p, Bounded c, Ord p, Ord c, Num c)
    => (Bool -> p -> [p] -> [p] -> q -> s -> ())
    -> Config p c -> Maybe (c,[p])
minPathTrace traceStep config@Config{start} =
    fix2
        (qSingleton @q @p @c start)
        (sSet @s @p @c @(p,c) (sEmpty @s @p @c @(p,c)) start (start,0))
        (stepPath' (Just traceStep) config)

{-# INLINE[1] stepPath' #-}                                             -- keep to reduce heap allocations
stepPath'
    :: forall q s p c. (Queue q p c, Storage s p c (p,c))
    => Maybe (Bool -> p -> [p] -> [p] -> q -> s -> ())                  -- current -> neighbors -> path -> q -> s -> s
    -> Config p c
    -> (q -> s -> Maybe (c,[p]))                                        -- loop
    -> q                                                                -- open
    -> s                                                                -- score storage
    -> Maybe (c,[p])
stepPath' mTraceStep Config{neighbors,cost,remaining,start,goal} =
    go
  where
    go :: (q -> s -> Maybe (c,[p])) -> q -> s -> Maybe (c,[p])
    go loop q s = qAlterMin @q @p @c q & \case
        (Nothing, _) -> Nothing                                         -- exhausted?
        (Just (p, _, _), qEjected) ->                                   -- point (candidate)
            let (_,pScoreStored) = sGet @s @p @c @(p,c) s (p, maxBound) p
                ns              = neighbors p
                (open2,s2)      = foldl' (addNeighbor s p pScoreStored) (qEjected,s) ns
                path            = sPath @s @p @c s2 start p
                -- TODO refactor
                follow          = maybe id (\t -> (t (goal p) p ns path open2 s2 `seq`)) mTraceStep
            in follow
                if | goal p    -> Just (pScoreStored, path)
                   | otherwise -> loop open2 s2

    addNeighbor :: s -> p -> c -> (q, s) -> p -> (q, s)
    addNeighbor s p pScoreStored (qAcc, sAcc) n =
        let nScoreNew        = pScoreStored + cost p n
            (_,nScoreStored) = sGet @s @p @c @(p,c) s (p, maxBound) n
        in if nScoreNew < nScoreStored
            then (qInsert qAcc n (maybe nScoreNew (\f -> nScoreNew + f n) remaining), sSet @s @p @c @(p,c) sAcc n (p,nScoreNew))
            else (qAcc,sAcc)

{-# INLINE[1] minScore' #-}                                             -- keep to reduce heap allocations
minScore' :: forall q s p c. (Queue q p c, Storage s p c c, Num c) => Config p c -> Maybe c
minScore' Config{neighbors,cost,remaining,start,goal} =
    fix2
        (qSingleton @q @p @c start)
        (sSet @s @p @c @c (sEmpty @s @p @c @c) start 0)
        \loop q s -> qAlterMin @q @p @c q & \case
            (Nothing, _) -> Nothing                                     -- exhausted?
            (Just (p, _, _), qEjected) ->
                let pScoreStored = sGet @s @p @c @c s maxBound p
                in if goal p                                            -- goal reached?
                    then Just pScoreStored
                    else uncurry loop $ foldl' (addNeighbor s p pScoreStored) (qEjected,s) (neighbors p)
  where
    addNeighbor :: s -> p -> c -> (q, s) -> p -> (q, s)
    addNeighbor s p pScoreStored (qAcc, sAcc) n =
        let nScoreNew    = pScoreStored + cost p n
            nScoreStored = sGet @s @p @c @c s  maxBound n
        in if nScoreNew < nScoreStored
            then (qInsert qAcc n (maybe nScoreNew (\f -> nScoreNew + f n) remaining), sSet @s @p @c @c sAcc n nScoreNew)
            else (qAcc,sAcc)

{-# INLINE[1] minScore #-}                                              -- keep to reduce heap allocations
minScore :: forall p c. (Ord p, Hashable p, Bounded c, Ord c, Num c) => Config p c -> Maybe c
minScore   = minScore' @(QH.HashPSQ p c ()) @(Map p c)

{-# INLINE[1] minScoreInt #-}                                           -- keep to reduce heap allocations
minScoreInt :: forall p c. (p ~ Int, Bounded c, Ord c, Num c) => Config p c -> Maybe c
minScoreInt = minScore' @(QI.IntPSQ c ()) @(IntMap c)


{-# INLINE[1] minScoreVec #-}                                           -- keep to reduce heap allocations
minScoreVec :: forall p c. (p ~ Int, VUM.Unbox c) => Int -> Config p c -> Maybe c
minScoreVec =
    go
  where
    go :: forall q . (q ~ QI.IntPSQ c ()) => Int -> Config p c -> Maybe c
    go scoreStorageSize Config{neighbors,cost,remaining,start,goal} = runST $ do
        let sMk     = VUM.replicate scoreStorageSize maxBound
            sRead   = VUM.read @(ST _)
            sWrite  = VUM.write

        s <- sMk
        qRef <- S.newSTRef $ qSingleton @q @p @c start
        sWrite s start 0

        fix \loop -> do
            S.readSTRef qRef <&> qAlterMin @q @p @c >>= \case
                (Nothing, _) -> pure Nothing                            -- exhausted?
                (Just (p, _, _), qEjected) -> do
                    (pScoreStored :: c) <- sRead s p
                    if goal p                                           -- goal reached?
                        then pure $ Just pScoreStored
                        else do
                            S.writeSTRef qRef qEjected
                            forM_ (neighbors p) \n -> do                -- for each neighbor
                                let nScoreNew = cost p n + pScoreStored
                                nScoreStored <- sRead s n
                                when (nScoreNew < nScoreStored) $ do    -- apply changes
                                    S.modifySTRef' qRef (\q -> qInsert q n (maybe nScoreNew (\f -> nScoreNew + f n) remaining))
                                    sWrite s n nScoreNew
                            loop
