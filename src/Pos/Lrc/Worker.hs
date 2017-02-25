{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Lrc.Worker
       ( lrcOnNewSlotWorker
       , lrcSingleShot
       , lrcSingleShotNoLock
       ) where

import           Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import           Control.Monad.Catch         (bracketOnError)
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import           Formatting                  (build, sformat, (%))
import           Mockable                    (fork)
import           Serokell.Util.Exceptions    ()
import           System.Wlog                 (logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic.Internal    (applyBlocksUnsafe, rollbackBlocksUnsafe,
                                              withBlkSemaphore_)
import           Pos.Communication.Protocol  (OutSpecs, WorkerSpec, localOnNewSlotWorker)
import           Pos.Constants               (slotSecurityParam)
import           Pos.Context                 (LrcSyncData, getNodeContext, ncLrcSync)
import qualified Pos.DB                      as DB
import qualified Pos.DB.GState               as GS
import           Pos.DB.Lrc                  (IssuersStakes, getLeaders, putEpoch,
                                              putIssuersStakes, putLeaders)
import           Pos.Lrc.Consumer            (LrcConsumer (..))
import           Pos.Lrc.Consumers           (allLrcConsumers)
import           Pos.Lrc.Error               (LrcError (..))
import           Pos.Lrc.FollowTheSatoshi    (followTheSatoshiM)
import           Pos.Lrc.Logic               (findAllRichmenMaybe)
import           Pos.Reporting               (reportMisbehaviourMasked)
import           Pos.Ssc.Class               (SscWorkersClass)
import           Pos.Ssc.Extra               (sscCalculateSeed)
import           Pos.Types                   (EpochIndex, EpochOrSlot (..),
                                              EpochOrSlot (..), HeaderHash, HeaderHash,
                                              SharedSeed, SlotId (..), StakeholderId,
                                              crucialSlot, epochIndexL, getEpochOrSlot,
                                              getEpochOrSlot)
import           Pos.Update.Poll.Types       (BlockVersionState (..))
import           Pos.Util                    (NewestFirst (..), logWarningWaitLinear,
                                              toOldestFirst)
import           Pos.WorkMode                (WorkMode)

lrcOnNewSlotWorker
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => (WorkerSpec m, OutSpecs)
lrcOnNewSlotWorker = localOnNewSlotWorker True $ \SlotId {..} ->
    when (siSlot < slotSecurityParam) $
    (lrcSingleShot siEpoch `catch` reportError) `catch` onLrcError
  where
    reportError (SomeException e) = do
        reportMisbehaviourMasked $ "Lrc worker failed with error: " <> show e
        throwM e
    onLrcError UnknownBlocksForLrc =
        logInfo
            "LRC worker can't do anything, because recent blocks aren't known"
    onLrcError e = throwM e

-- | Run leaders and richmen computation for given epoch. If stable
-- block for this epoch is not known, LrcError will be thrown.
lrcSingleShot
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => EpochIndex -> m ()
lrcSingleShot epoch = lrcSingleShotImpl True epoch allLrcConsumers

-- | Same, but doesn't take lock on the semaphore.
lrcSingleShotNoLock
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => EpochIndex -> m ()
lrcSingleShotNoLock epoch = lrcSingleShotImpl False epoch allLrcConsumers

lrcSingleShotImpl
    :: WorkMode ssc m
    => Bool -> EpochIndex -> [LrcConsumer m] -> m ()
lrcSingleShotImpl withSemaphore epoch consumers = do
    lock <- ncLrcSync <$> getNodeContext
    tryAcuireExclusiveLock epoch lock onAcquiredLock
  where
    onAcquiredLock = do
        (need, filteredConsumers) <-
            logWarningWaitLinear 5 "determining whether LRC is needed" $ do
                expectedRichmenComp <-
                    filterM (flip lcIfNeedCompute epoch) consumers
                needComputeLeaders <- isNothing <$> getLeaders epoch
                let needComputeRichmen = not . null $ expectedRichmenComp
                when needComputeLeaders $ logInfo "Need to compute leaders"
                when needComputeRichmen $ logInfo "Need to compute richmen"
                return $
                    ( needComputeLeaders || needComputeRichmen
                    , expectedRichmenComp)
        when need $ do
            logInfo "LRC is starting"
            if withSemaphore
                then withBlkSemaphore_ $ lrcDo epoch filteredConsumers
            -- we don't change/use it in lcdDo in fact
                else void . lrcDo epoch filteredConsumers =<< GS.getTip
            logInfo "LRC has finished"
        putEpoch epoch
        logInfo "LRC has updated LRC DB"

tryAcuireExclusiveLock
    :: (MonadMask m, MonadIO m)
    => EpochIndex -> TVar LrcSyncData -> m () -> m ()
tryAcuireExclusiveLock epoch lock action =
    bracketOnError acquireLock (flip whenJust releaseLock) doAction
  where
    acquireLock = atomically $ do
        res <- readTVar lock
        case res of
            (False, _) -> retry
            (True, lockEpoch)
                | lockEpoch >= epoch -> pure Nothing
                | lockEpoch == epoch - 1 ->
                    Just lockEpoch <$ writeTVar lock (False, lockEpoch)
                | otherwise -> throwM UnknownBlocksForLrc
    releaseLock = atomically . writeTVar lock . (True,)
    doAction Nothing = pass
    doAction _       = action >> releaseLock epoch

lrcDo
    :: WorkMode ssc m
    => EpochIndex -> [LrcConsumer m] -> HeaderHash -> m HeaderHash
lrcDo epoch consumers tip = tip <$ do
    blundsUpToGenesis <- DB.loadBlundsFromTipWhile upToGenesis
    -- If there are blocks from 'epoch' it means that we somehow accepted them
    -- before running LRC for 'epoch'. It's very bad.
    unless (null blundsUpToGenesis) $ throwM LrcAfterGenesis
    NewestFirst blundsList <- DB.loadBlundsFromTipWhile whileAfterCrucial
    case nonEmpty blundsList of
        Nothing -> throwM UnknownBlocksForLrc
        Just (NewestFirst -> blunds) -> do
            mbSeed <- sscCalculateSeed epoch
            case mbSeed of
                Left e ->
                    -- FIXME: don't panic, use previous seed!
                    panic $ sformat ("SSC couldn't compute seed: " %build) e
                Right seed -> do
                    rollbackBlocksUnsafe blunds
                    compute seed `finally` applyBack (toOldestFirst blunds)
  where
    applyBack blunds = applyBlocksUnsafe blunds Nothing
    upToGenesis b = b ^. epochIndexL >= epoch
    whileAfterCrucial b = getEpochOrSlot b > crucial
    crucial = EpochOrSlot $ Right $ crucialSlot epoch
    compute seed = do
        issuersComputationDo epoch
        richmenComputationDo epoch consumers
        DB.sanityCheckDB
        leadersComputationDo epoch seed

issuersComputationDo :: forall ssc m . WorkMode ssc m => EpochIndex -> m ()
issuersComputationDo epochId = do
    issuers <- unionHSs .
               map (bvsIssuersStable . snd) <$>
               GS.getConfirmedBVStates
    issuersStakes <- foldM putIsStake mempty issuers
    putIssuersStakes epochId issuersStakes
  where
    unionHSs = foldl' (flip HS.union) mempty
    putIsStake :: IssuersStakes -> StakeholderId -> m IssuersStakes
    putIsStake hm id = GS.getFtsStake id >>= \case
        Nothing ->
           hm <$ (logWarning $ sformat ("Stake for issuer "%build% " not found") id)
        Just stake -> pure $ HM.insert id stake hm

leadersComputationDo :: WorkMode ssc m => EpochIndex -> SharedSeed -> m ()
leadersComputationDo epochId seed =
    unlessM (isJust <$> getLeaders epochId) $ do
        totalStake <- GS.getTotalFtsStake
        leaders <- GS.runBalanceIterator (followTheSatoshiM seed totalStake)
        putLeaders epochId leaders

richmenComputationDo :: forall ssc m . WorkMode ssc m
    => EpochIndex -> [LrcConsumer m] -> m ()
richmenComputationDo epochIdx consumers = unless (null consumers) $ do
    total <- GS.getTotalFtsStake
    let minThreshold = safeThreshold total (not . lcConsiderDelegated)
    let minThresholdD = safeThreshold total lcConsiderDelegated
    (richmen, richmenD) <- GS.runBalanceIterator
                               (findAllRichmenMaybe @ssc minThreshold minThresholdD)
    let callCallback cons = void $ fork $
            if lcConsiderDelegated cons
            then lcComputedCallback cons epochIdx total
                   (HM.filter (>= lcThreshold cons total) richmenD)
            else lcComputedCallback cons epochIdx total
                   (HM.filter (>= lcThreshold cons total) richmen)
    mapM_ callCallback consumers
  where
    safeThreshold total f =
        safeMinimum
        $ map (flip lcThreshold total)
        $ filter f consumers
    safeMinimum a = if null a then Nothing else Just $ minimum a
