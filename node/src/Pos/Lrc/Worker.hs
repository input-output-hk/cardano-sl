{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Lrc.Worker
       ( LrcModeFullNoSemaphore
       , LrcModeFull
       , lrcOnNewSlotWorker
       , lrcSingleShot
       , lrcSingleShotNoLock
       ) where

import           Universum

import           Control.Lens               (views)
import           Control.Monad.Catch        (bracketOnError)
import           Control.Monad.STM          (retry)
import           Data.Conduit               (runConduitRes, (.|))
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, sformat, (%))
import           Mockable                   (forConcurrently)
import           Serokell.Util.Exceptions   ()
import           System.Wlog                (logDebug, logInfo, logWarning)

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic.Internal   (BypassSecurityCheck (..), MonadBlockApply,
                                             applyBlocksUnsafe, rollbackBlocksUnsafe)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker)
import           Pos.Context                (recoveryCommGuard)
import           Pos.Core                   (Coin, EpochIndex, EpochOrSlot (..),
                                             EpochOrSlot (..), SharedSeed, SlotId (..),
                                             StakeholderId, crucialSlot, epochIndexL,
                                             getEpochOrSlot, getSlotIndex,
                                             slotSecurityParam)
import qualified Pos.DB.DB                  as DB
import qualified Pos.GState                 as GS
import           Pos.Infra.Semaphore        (BlkSemaphore, withBlkSemaphore)
import           Pos.Lrc.Consumer           (LrcConsumer (..))
import           Pos.Lrc.Consumers          (allLrcConsumers)
import           Pos.Lrc.Context            (LrcContext (lcLrcSync), LrcSyncData (..))
import           Pos.Lrc.DB                 (IssuersStakes, getLeaders, getSeed, putEpoch,
                                             putIssuersStakes, putLeaders, putSeed)
import           Pos.Lrc.Error              (LrcError (..))
import           Pos.Lrc.Fts                (followTheSatoshiM)
import           Pos.Lrc.Logic              (findAllRichmenMaybe)
import           Pos.Lrc.Mode               (LrcMode)
import           Pos.Reporting              (reportMisbehaviourSilent)
import           Pos.Slotting               (MonadSlots)
import           Pos.Ssc.Class              (SscHelpersClass, SscWorkersClass)
import           Pos.Ssc.Extra              (MonadSscMem, sscCalculateSeed)
import           Pos.Update.DB              (getCompetingBVStates)
import           Pos.Update.Poll.Types      (BlockVersionState (..))
import           Pos.Util                   (logWarningWaitLinear, maybeThrow)
import           Pos.Util.Chrono            (NewestFirst (..), toOldestFirst)
import           Pos.WorkMode.Class         (WorkMode)

lrcOnNewSlotWorker
    :: forall ssc ctx m.
       (WorkMode ssc ctx m, SscWorkersClass ssc)
    => (WorkerSpec m, OutSpecs)
lrcOnNewSlotWorker = localOnNewSlotWorker True $ \SlotId {..} ->
    recoveryCommGuard $
        when (getSlotIndex siSlot < fromIntegral slotSecurityParam) $
            lrcSingleShot @ssc @ctx siEpoch `catch` onLrcError
  where
    -- Here we log it as a warning and report an error, even though it
    -- can happen there we don't know recent blocks. That's because if
    -- we don't know them, we should be in recovery mode and this
    -- worker should be turned off.
    onLrcError e@UnknownBlocksForLrc = do
        reportError e
        logWarning
            "LRC worker can't do anything, because recent blocks aren't known"
    onLrcError e = reportError e >> throwM e
    -- FIXME [CSL-1340]: it should be reported as 'RError'.
    reportError e =
        reportMisbehaviourSilent False $
        "Lrc worker failed with error: " <> show e

type LrcModeFullNoSemaphore ssc ctx m =
    ( LrcMode ssc ctx m
    , SscWorkersClass ssc
    , SscHelpersClass ssc
    , MonadSscMem ssc ctx m
    , MonadSlots ctx m
    , MonadBlockApply ssc ctx m
    , MonadReader ctx m
    )

-- | 'LrcModeFull' contains all constraints necessary to launch LRC.
type LrcModeFull ssc ctx m =
    ( LrcModeFullNoSemaphore ssc ctx m
    , HasLens BlkSemaphore ctx BlkSemaphore
    )

type WithBlkSemaphore_ m = m () -> m ()

-- | Run leaders and richmen computation for given epoch. If stable
-- block for this epoch is not known, LrcError will be thrown.
lrcSingleShot
    :: forall ssc ctx m. (LrcModeFull ssc ctx m)
    => EpochIndex -> m ()
lrcSingleShot epoch =
    lrcSingleShotImpl @ssc (withBlkSemaphore . const) epoch (allLrcConsumers @ssc)

-- | Same, but doesn't take lock on the semaphore.
lrcSingleShotNoLock
    :: forall ssc ctx m. (LrcModeFullNoSemaphore ssc ctx m)
    => EpochIndex -> m ()
lrcSingleShotNoLock epoch =
    lrcSingleShotImpl @ssc identity epoch (allLrcConsumers @ssc)

lrcSingleShotImpl
    :: forall ssc ctx m. (LrcModeFullNoSemaphore ssc ctx m)
    => WithBlkSemaphore_ m -> EpochIndex -> [LrcConsumer m] -> m ()
lrcSingleShotImpl withSemaphore epoch consumers = do
    lock <- views (lensOf @LrcContext) lcLrcSync
    tryAcquireExclusiveLock epoch lock onAcquiredLock
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
            withSemaphore $ lrcDo @ssc epoch filteredConsumers
            logInfo "LRC has finished"
        putEpoch epoch
        logInfo "LRC has updated LRC DB"

tryAcquireExclusiveLock
    :: (MonadMask m, MonadIO m)
    => EpochIndex -> TVar LrcSyncData -> m () -> m ()
tryAcquireExclusiveLock epoch lock action =
    bracketOnError acquireLock (flip whenJust releaseLock) doAction
  where
    acquireLock = atomically $ do
        sync <- readTVar lock
        if | not (lrcNotRunning sync) {- i.e. lrc is running -} -> retry
           | lastEpochWithLrc sync >= epoch -> pure Nothing
           | lastEpochWithLrc sync == epoch - 1 -> do
                 writeTVar lock (LrcSyncData False (lastEpochWithLrc sync))
                 pure (Just (lastEpochWithLrc sync))
           | otherwise -> throwM UnknownBlocksForLrc
    releaseLock e = atomically $ writeTVar lock (LrcSyncData True e)
    doAction Nothing = pass
    doAction _       = action >> releaseLock epoch

lrcDo
    :: forall ssc ctx m.
       LrcModeFullNoSemaphore ssc ctx m
    => EpochIndex -> [LrcConsumer m] -> m ()
lrcDo epoch consumers = do
    blundsUpToGenesis <- DB.loadBlundsFromTipWhile @ssc upToGenesis
    -- If there are blocks from 'epoch' it means that we somehow accepted them
    -- before running LRC for 'epoch'. It's very bad.
    unless (null blundsUpToGenesis) $ throwM LrcAfterGenesis
    -- We don't calculate the seed inside 'withBlocksRolledBack' because
    -- there are shares in those ~2k blocks that 'withBlocksRolledBack'
    -- rolls back.
    seed <- sscCalculateSeed @ssc epoch >>= \case
        Right s -> do
            logInfo $ sformat
                ("Calculated seed for epoch "%build%" successfully") epoch
            return s
        Left err -> do
            logWarning $ sformat
                ("SSC couldn't compute seed: "%build%" for epoch "%build%
                 ", going to reuse seed for previous epoch")
                err epoch
            getSeed (epoch - 1) >>=
                maybeThrow (CanNotReuseSeedForLrc (epoch - 1))
    putSeed epoch seed
    -- Roll back to the crucial slot and calculate richmen, etc.
    NewestFirst blundsList <- DB.loadBlundsFromTipWhile whileAfterCrucial
    case nonEmpty blundsList of
        Nothing -> throwM UnknownBlocksForLrc
        Just (NewestFirst -> blunds) ->
            withBlocksRolledBack blunds $ do
                issuersComputationDo epoch
                richmenComputationDo epoch consumers
                DB.sanityCheckDB
                leadersComputationDo epoch seed
  where
    applyBack blunds = applyBlocksUnsafe blunds Nothing
    upToGenesis b = b ^. epochIndexL >= epoch
    whileAfterCrucial b = getEpochOrSlot b > crucial
    crucial = EpochOrSlot $ Right $ crucialSlot epoch
    bsc =
        -- LRC rollbacks temporarily to examine the state of the DB at the
        -- time of the crucial slot. The crucial slot may be further than 'blkSecurityParam'
        -- slots from the current one, so the security check must be disabled.
        BypassSecurityCheck True
    withBlocksRolledBack blunds =
        bracket_ (rollbackBlocksUnsafe bsc blunds)
                 (applyBack (toOldestFirst blunds))

issuersComputationDo :: forall ssc ctx m . LrcMode ssc ctx m => EpochIndex -> m ()
issuersComputationDo epochId = do
    issuers <- unionHSs .
               map (bvsIssuersStable . snd) <$>
               getCompetingBVStates
    issuersStakes <- foldM putIsStake mempty issuers
    putIssuersStakes epochId issuersStakes
  where
    unionHSs = foldl' (flip HS.union) mempty
    putIsStake :: IssuersStakes -> StakeholderId -> m IssuersStakes
    putIsStake hm id = GS.getRealStake id >>= \case
        Nothing ->
           hm <$ (logWarning $ sformat ("Stake for issuer "%build% " not found") id)
        Just stake -> pure $ HM.insert id stake hm

leadersComputationDo :: LrcMode ssc ctx m => EpochIndex -> SharedSeed -> m ()
leadersComputationDo epochId seed =
    unlessM (isJust <$> getLeaders epochId) $ do
        totalStake <- GS.getRealTotalStake
        leaders <- runConduitRes $ GS.balanceSource .| followTheSatoshiM seed totalStake
        putLeaders epochId leaders

richmenComputationDo
    :: forall ssc ctx m.
       LrcMode ssc ctx m
    => EpochIndex -> [LrcConsumer m] -> m ()
richmenComputationDo epochIdx consumers = unless (null consumers) $ do
    total <- GS.getRealTotalStake
    logDebug $ "Effective total stake: " <> pretty total
    consumersAndThds <-
        zip consumers <$> mapM (flip lcThreshold total) consumers
    let minThreshold :: Maybe Coin
        minThreshold = safeThreshold consumersAndThds (not . lcConsiderDelegated)
        minThresholdD :: Maybe Coin
        minThresholdD = safeThreshold consumersAndThds lcConsiderDelegated
    (richmen, richmenD) <- runConduitRes $
        GS.balanceSource .| findAllRichmenMaybe minThreshold minThresholdD
    logDebug $ "Size of richmen: " <> show (HM.size richmen)
    logDebug $ "Size of richmenD: " <> show (HM.size richmenD)
    let callCallback (cons, thd) =
            if lcConsiderDelegated cons
            then lcComputedCallback cons epochIdx total
                   (HM.filter (>= thd) richmenD)
            else lcComputedCallback cons epochIdx total
                   (HM.filter (>= thd) richmen)
    void $ forConcurrently consumersAndThds callCallback
  where
    safeThreshold consumersAndThds f =
        safeMinimum $ map snd $ filter (f . fst) consumersAndThds
    safeMinimum a
        | null a = Nothing
        | otherwise = Just $ minimum a
