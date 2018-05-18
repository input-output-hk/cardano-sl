{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Workers responsible for Leaders and Richmen computation.
-- This module also contains high-level logic of LRC for historical
-- reasons.
-- And actually nowadays there are no workers here.

module Pos.Lrc.Worker
       ( LrcModeFull
       , lrcSingleShot
       ) where

import           Universum

import           Control.Exception.Safe (bracketOnError)
import           Control.Lens (views)
import           Control.Monad.STM (retry)
import           Data.Coerce (coerce)
import           Data.Conduit (runConduitRes, (.|))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Formatting (build, ords, sformat, (%))
import           Mockable (forConcurrently)
import qualified System.Metrics.Counter as Metrics
import           System.Wlog (logDebug, logInfo, logWarning)

import           Pos.Block.Logic.Internal (BypassSecurityCheck (..), MonadBlockApply,
                                           applyBlocksUnsafe, rollbackBlocksUnsafe)
import           Pos.Block.Slog.Logic (ShouldCallBListener (..))
import           Pos.Core (Coin, EpochIndex, EpochOrSlot (..), SharedSeed, StakeholderId,
                           blkSecurityParam, crucialSlot, epochIndexL, getEpochOrSlot,
                           HasGenesisBlockVersionData, HasGenesisData, HasGenesisHash,
                           HasGeneratedSecrets, HasProtocolConstants, HasProtocolMagic)
import qualified Pos.DB.Block.Load as DB
import qualified Pos.DB.GState.Stakes as GS (getRealStake, getRealTotalStake)
import qualified Pos.GState.SanityCheck as DB (sanityCheckDB)
import           Pos.Lrc.Consumer (LrcConsumer (..))
import           Pos.Lrc.Consumers (allLrcConsumers)
import           Pos.Lrc.Context (LrcContext (lcLrcSync), LrcSyncData (..))
import           Pos.Lrc.DB (IssuersStakes, getSeed, putEpoch, putIssuersStakes, putSeed)
import qualified Pos.Lrc.DB as LrcDB (hasLeaders, putLeadersForEpoch)
import           Pos.Lrc.Error (LrcError (..))
import           Pos.Lrc.Fts (followTheSatoshiM)
import           Pos.Lrc.Logic (findAllRichmenMaybe)
import           Pos.Lrc.Mode (LrcMode)
import           Pos.Reporting.MemState (HasMisbehaviorMetrics (..), MisbehaviorMetrics (..))
import           Pos.Slotting (MonadSlots)
import           Pos.Ssc (MonadSscMem, noReportNoSecretsForEpoch1, sscCalculateSeed)
import           Pos.Ssc.Message (SscMessageConstraints)
import qualified Pos.Txp.DB.Stakes as GS (stakeSource)
import           Pos.Update.DB (getCompetingBVStates)
import           Pos.Update.Poll.Types (BlockVersionState (..))
import           Pos.Util (maybeThrow)
import           Pos.Util.Chrono (NE, NewestFirst (..), toOldestFirst)
import           Pos.Util.TimeLimit (logWarningWaitLinear)
import           Pos.Util.Util (HasLens (..))


----------------------------------------------------------------------------
-- Single shot
----------------------------------------------------------------------------

-- | 'LrcModeFull' contains all constraints necessary to launch LRC.
type LrcModeFull ctx m =
    ( LrcMode ctx m
    , MonadSscMem ctx m
    , MonadSlots ctx m
    , MonadBlockApply ctx m
    , MonadReader ctx m
    , SscMessageConstraints
    )

-- | Run leaders and richmen computation for given epoch. If stable
-- block for this epoch is not known, LrcError will be thrown.
-- It assumes that 'StateLock' is taken already.
lrcSingleShot
    :: forall ctx m.
       ( LrcModeFull ctx m
       , HasGeneratedSecrets
       , HasGenesisBlockVersionData
       , HasProtocolConstants
       , HasProtocolMagic
       , HasGenesisData
       , HasGenesisHash
       , HasMisbehaviorMetrics ctx
       )
    => EpochIndex -> m ()
lrcSingleShot epoch = do
    lock <- views (lensOf @LrcContext) lcLrcSync
    logDebug $ sformat
        ("lrcSingleShot is trying to acquire LRC lock, the epoch is "
         %build) epoch
    tryAcquireExclusiveLock epoch lock onAcquiredLock
  where
    consumers = allLrcConsumers @ctx @m
    for_thEpochMsg = sformat (" for "%ords%" epoch") epoch
    onAcquiredLock = do
        logDebug "lrcSingleShot has acquired LRC lock"
        (need, filteredConsumers) <-
            logWarningWaitLinear 5 "determining whether LRC is needed" $ do
                expectedRichmenComp <-
                    filterM (flip lcIfNeedCompute epoch) consumers
                needComputeLeaders <- not <$> LrcDB.hasLeaders epoch
                let needComputeRichmen = not . null $ expectedRichmenComp
                when needComputeLeaders $ logInfo
                    ("Need to compute leaders" <> for_thEpochMsg)
                when needComputeRichmen $ logInfo
                    ("Need to compute richmen" <> for_thEpochMsg)
                return $
                    ( needComputeLeaders || needComputeRichmen
                    , expectedRichmenComp)
        when need $ do
            logInfo "LRC is starting actual computation"
            lrcDo epoch filteredConsumers
            logInfo "LRC has finished actual computation"
        putEpoch epoch
        logInfo ("LRC has updated LRC DB" <> for_thEpochMsg)

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
    :: forall ctx m.
       ( LrcModeFull ctx m
       , HasGeneratedSecrets
       , HasGenesisBlockVersionData
       , HasProtocolConstants
       , HasProtocolMagic
       , HasGenesisData
       , HasGenesisHash
       , HasMisbehaviorMetrics ctx
       )
    => EpochIndex -> [LrcConsumer m] -> m ()
lrcDo epoch consumers = do
    blundsUpToGenesis <- DB.loadBlundsFromTipWhile upToGenesis
    -- If there are blocks from 'epoch' it means that we somehow accepted them
    -- before running LRC for 'epoch'. It's very bad.
    unless (null blundsUpToGenesis) $ throwM LrcAfterGenesis
    -- We don't calculate the seed inside 'withBlocksRolledBack' because
    -- there are shares in those ~2k blocks that 'withBlocksRolledBack'
    -- rolls back.
    --
    -- However, it's important to check that there are blocks to
    -- rollback before computing ssc seed (because if there are no
    -- blocks, it doesn't make sense to do it).
    blundsToRollback <- DB.loadBlundsFromTipWhile whileAfterCrucial
    blundsToRollbackNE <-
        maybeThrow UnknownBlocksForLrc (atLeastKNewestFirst blundsToRollback)
    seed <- sscCalculateSeed epoch >>= \case
        Right s -> do
            logInfo $ sformat
                ("Calculated seed for epoch "%build%" successfully") epoch
            return s
        Left _ -> do
            -- Critical error means that the system is in dangerous state.
            -- For now let's consider all errors critical, maybe we'll revise it later.
            unless (noReportNoSecretsForEpoch1 && epoch == 1) $ do
                whenJustM (view misbehaviorMetrics) $ liftIO .
                    Metrics.inc . _mmSscFailures
            getSeed (epoch - 1) >>=
                maybeThrow (CanNotReuseSeedForLrc (epoch - 1))
    putSeed epoch seed
    -- Roll back to the crucial slot and calculate richmen, etc.
    withBlocksRolledBack blundsToRollbackNE $ do
        issuersComputationDo epoch
        richmenComputationDo epoch consumers
        DB.sanityCheckDB
        leadersComputationDo epoch seed
  where
    atLeastKNewestFirst :: forall a. NewestFirst [] a -> Maybe (NewestFirst NE a)
    atLeastKNewestFirst l =
        if length l >= fromIntegral blkSecurityParam
        then coerce (nonEmpty @a) l
        else Nothing

    applyBack blunds = applyBlocksUnsafe scb blunds Nothing
    upToGenesis b = b ^. epochIndexL >= epoch
    whileAfterCrucial b = getEpochOrSlot b > crucial
    crucial = EpochOrSlot $ Right $ crucialSlot epoch
    bsc =
        -- LRC rollbacks temporarily to examine the state of the DB at the
        -- time of the crucial slot. The crucial slot may be further than 'blkSecurityParam'
        -- slots from the current one, so the security check must be disabled.
        BypassSecurityCheck True
    scb =
        -- We don't want to trigger BListener callback because
        -- LRC computation via rollback is an artificial solution
        -- and outer viewers mustn't know about it.
        ShouldCallBListener False
    withBlocksRolledBack blunds =
        bracket_ (rollbackBlocksUnsafe bsc scb blunds)
                 (applyBack (toOldestFirst blunds))

issuersComputationDo :: forall ctx m . LrcMode ctx m => EpochIndex -> m ()
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

leadersComputationDo ::
       forall ctx m. (LrcMode ctx m, HasProtocolConstants)
    => EpochIndex
    -> SharedSeed
    -> m ()
leadersComputationDo epochId seed =
    unlessM (LrcDB.hasLeaders epochId) $ do
        totalStake <- GS.getRealTotalStake
        leaders <-
            runConduitRes $ GS.stakeSource .| followTheSatoshiM seed totalStake
        LrcDB.putLeadersForEpoch epochId leaders

richmenComputationDo
    :: forall ctx m.
       LrcMode ctx m
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
        GS.stakeSource .| findAllRichmenMaybe minThreshold minThresholdD
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

----------------------------------------------------------------------------
-- Worker
----------------------------------------------------------------------------

-- TODO CSL-359
--
-- This worker no longer exists. It was added as the first step
-- towards CSL-359. The idea is that we can start LRC before the end
-- of an epoch. We just need to check that rollback deeper than
-- 'crucialSlot' didn't happen. It's only an optimization which can be
-- quite crucial when there are many stakeholders.
--
-- It was deleted because of possible deadlock. This worker may start
-- doing LRC and try to acquire 'StateLock' while another thread may
-- hold 'StateLock' for block processing and try to start LRC.  So if
-- you are going to bring it back at some point, please take it into
-- account.  One way to avoid locking here is to do CSL-360, i. e. use
-- snapshot instead of locking.
--
-- You can find it in git history if you want to.
