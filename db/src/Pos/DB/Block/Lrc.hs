{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Workers responsible for Leaders and Richmen computation.
-- This module also contains high-level logic of LRC for historical
-- reasons.
-- And actually nowadays there are no workers here.

module Pos.DB.Block.Lrc
       ( LrcModeFull
       , lrcSingleShot
       ) where

import           Universum hiding (id)

import           Control.Exception.Safe (bracketOnError)
import           Control.Lens (views)
import           Control.Monad.STM (retry)
import           Data.Coerce (coerce)
import           Data.Conduit (ConduitT, runConduitRes, (.|))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Formatting (build, sformat, (%))
import qualified System.Metrics.Counter as Metrics
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBlkSecurityParam, configBlockVersionData,
                     configEpochSlots, configK)
import           Pos.Chain.Lrc (LrcError (..), RichmenStakes,
                     findDelegationStakes, findRichmenStakes,
                     followTheSatoshiM)
import           Pos.Chain.Ssc (MonadSscMem, noReportNoSecretsForEpoch1)
import           Pos.Chain.Update (BlockVersionState (..))
import           Pos.Core (Coin, EpochIndex (..), EpochOrSlot (..), SharedSeed,
                     SlotCount, StakeholderId, crucialSlot, epochIndexL,
                     getEpochOrSlot)
import           Pos.Core.Chrono (NE, NewestFirst (..), toOldestFirst)
import           Pos.Core.Conc (forConcurrently)
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..),
                     MisbehaviorMetrics (..))
import           Pos.Core.Slotting (MonadSlots)
import           Pos.Core.Util.TimeLimit (logWarningWaitLinear)
import qualified Pos.DB.Block.GState.SanityCheck as DB (sanityCheckDB)
import qualified Pos.DB.Block.Load as DB
import           Pos.DB.Block.Logic.Internal (BypassSecurityCheck (..),
                     MonadBlockApply, applyBlocksUnsafe, rollbackBlocksUnsafe)
import           Pos.DB.Block.Slog.Logic (ShouldCallBListener (..))
import           Pos.DB.Class (MonadDBRead, MonadGState)
import           Pos.DB.Delegation (getDelegators, isIssuerByAddressHash)
import qualified Pos.DB.GState.Stakes as GS (getRealStake, getRealTotalStake)
import           Pos.DB.Lrc (IssuersStakes, LrcConsumer (..), LrcContext (..),
                     LrcMode, LrcSyncData (..), allLrcConsumers, getSeed,
                     putEpoch, putIssuersStakes, putSeed)
import qualified Pos.DB.Lrc as LrcDB (hasLeaders, putLeadersForEpoch)
import           Pos.DB.Ssc (sscCalculateSeed)
import qualified Pos.DB.Txp.Stakes as GS
import           Pos.DB.Update (getCompetingBVStates)
import           Pos.Util (maybeThrow)
import           Pos.Util.Util (HasLens (..), intords)
import           Pos.Util.Wlog (logDebug, logInfo, logWarning)


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
    )

-- | Run leaders and richmen computation for given epoch. If stable
-- block for this epoch is not known, LrcError will be thrown.
-- It assumes that 'StateLock' is taken already.
lrcSingleShot
    :: forall ctx m
     . (LrcModeFull ctx m, HasMisbehaviorMetrics ctx)
    => Genesis.Config
    -> EpochIndex
    -> m ()
lrcSingleShot genesisConfig epoch = do
    lock <- views (lensOf @LrcContext) lcLrcSync
    logDebug $ sformat
        ("lrcSingleShot is trying to acquire LRC lock, the epoch is "
         %build) epoch
    tryAcquireExclusiveLock epoch lock onAcquiredLock
  where
    consumers = allLrcConsumers @ctx @m (configBlockVersionData genesisConfig)
    for_thEpochMsg = sformat (" for "%intords%" epoch") (getEpochIndex epoch)
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
            lrcDo genesisConfig epoch filteredConsumers
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
    :: forall ctx m
     . ( LrcModeFull ctx m
       , HasMisbehaviorMetrics ctx
       )
    => Genesis.Config
    -> EpochIndex
    -> [LrcConsumer m]
    -> m ()
lrcDo genesisConfig epoch consumers = do
    blundsUpToGenesis <- DB.loadBlundsFromTipWhile genesisHash upToGenesis
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
    blundsToRollback <- DB.loadBlundsFromTipWhile genesisHash whileAfterCrucial
    blundsToRollbackNE <-
        maybeThrow UnknownBlocksForLrc (atLeastKNewestFirst blundsToRollback)
    seed <- sscCalculateSeed (configBlockVersionData genesisConfig) epoch >>= \case
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
        DB.sanityCheckDB $ configGenesisData genesisConfig
        leadersComputationDo (configEpochSlots genesisConfig) epoch seed
  where
    genesisHash = configGenesisHash genesisConfig
    atLeastKNewestFirst :: forall a. NewestFirst [] a -> Maybe (NewestFirst NE a)
    atLeastKNewestFirst l =
        if length l >= configK genesisConfig
        then coerce (nonEmpty @a) l
        else Nothing

    applyBack blunds = applyBlocksUnsafe genesisConfig scb blunds Nothing
    upToGenesis b = b ^. epochIndexL >= epoch
    whileAfterCrucial b = getEpochOrSlot b > crucial
    crucial = EpochOrSlot $ Right $ crucialSlot
        (configBlkSecurityParam genesisConfig)
        epoch
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
        bracket_ (rollbackBlocksUnsafe genesisConfig bsc scb blunds)
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

leadersComputationDo :: LrcMode ctx m
    => SlotCount
    -> EpochIndex
    -> SharedSeed
    -> m ()
leadersComputationDo epochSlots epochId seed =
    unlessM (LrcDB.hasLeaders epochId) $ do
        totalStake <- GS.getRealTotalStake
        leaders <-
            runConduitRes $ GS.stakeSource .| followTheSatoshiM epochSlots seed totalStake
        LrcDB.putLeadersForEpoch epochSlots epochId leaders

--------------------------------------------------------------------------------
-- Richmen
--------------------------------------------------------------------------------

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

type MonadDBReadFull m = (MonadDBRead m, MonadGState m, MonadUnliftIO m)

-- Can it be improved using conduits?
-- | Find delegated richmen using precomputed usual richmen.
-- Do it using one pass by delegation DB.
findDelRichUsingPrecomp
    :: forall m.
       (MonadDBReadFull m)
    => RichmenStakes -> Coin -> m RichmenStakes
findDelRichUsingPrecomp precomputed thr = do
    (old, new) <-
        runConduitRes $
        getDelegators .|
        findDelegationStakes isIssuerByAddressHash GS.getRealStake thr
    -- attention: order of new and precomputed is important
    -- we want to use new stakes (computed from delegated) of precomputed richmen
    pure (new `HM.union` (precomputed `HM.difference` (HS.toMap old)))

-- | Find delegated richmen.
findDelegatedRichmen
    :: (MonadDBReadFull m)
    => Coin -> ConduitT (StakeholderId, Coin) Void m RichmenStakes
findDelegatedRichmen thr = do
    st <- findRichmenStakes thr
    lift $ findDelRichUsingPrecomp st thr

-- | Function considers all variants of computation
-- and compute using one pass by stake DB and one pass by delegation DB.
findAllRichmenMaybe
    :: forall m.
       (MonadDBReadFull m)
    => Maybe Coin -- ^ Eligibility threshold (optional)
    -> Maybe Coin -- ^ Delegation threshold (optional)
    -> ConduitT (StakeholderId, Coin) Void m (RichmenStakes, RichmenStakes)
findAllRichmenMaybe maybeT maybeTD
    | Just t <- maybeT
    , Just tD <- maybeTD = do
        let mn = min t tD
        richmenMin <- findRichmenStakes mn
        let richmen = HM.filter (>= t) richmenMin
        let precomputedD = HM.filter (>= tD) richmenMin
        richmenD <- lift $ findDelRichUsingPrecomp precomputedD tD
        pure (richmen, richmenD)
    | Just t <- maybeT = (,mempty) <$> findRichmenStakes t
    | Just tD <- maybeTD = (mempty,) <$> findDelegatedRichmen tD
    | otherwise = pure (mempty, mempty)

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
