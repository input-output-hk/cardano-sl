{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Higher-level logic of SSC independent of concrete SSC.

module Pos.Ssc.Extra.Logic
       (
         -- * Utilities
         sscRunLocalQuery
       , sscRunLocalSTM
       , sscRunGlobalQuery

         -- * Seed calculation
       , sscCalculateSeed

         -- * Local Data
       , sscGetLocalPayload
       , sscNormalize
       , sscResetLocal

         -- * GState
       , sscApplyBlocks
       , sscRollbackBlocks
       , sscVerifyBlocks
       ) where

import           Universum

import           Control.Lens             (_Wrapped)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Morph      (hoist)
import           Control.Monad.State      (get, put)
import qualified Crypto.Random            as Rand
import           Formatting               (build, int, sformat, (%))
import           Serokell.Util            (listJson)
import           System.Wlog              (NamedPureLogger, WithLogger,
                                           launchNamedPureLog, logDebug)

import           Pos.Core                 (EpochIndex, HeaderHash, IsHeader, SharedSeed,
                                           SlotId, epochIndexL, headerHash)
import           Pos.DB                   (MonadBlockDBGeneric, MonadDBRead, MonadGState,
                                           SomeBatchOp, gsAdoptedBVData)
import           Pos.DB.GState.Common     (getTipHeaderGeneric)
import           Pos.Exception            (assertionFailed)
import           Pos.Lrc.Context          (HasLrcContext, lrcActionOnEpochReason)
import           Pos.Lrc.Types            (RichmenStakes)
import           Pos.Reporting            (MonadReporting, reportError)
import           Pos.Slotting.Class       (MonadSlots)
import           Pos.Ssc.Class.LocalData  (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage    (SscGStateClass (..))
import           Pos.Ssc.Core             (SscPayload)
import           Pos.Ssc.Extra.Class      (MonadSscMem, askSscMem)
import           Pos.Ssc.Types            (SscBlock, SscState (sscGlobal, sscLocal),
                                           SscGlobalState, SscLocalData)
import           Pos.Ssc.RichmenComponent (getRichmenSsc)
import           Pos.Ssc.SeedError        (SscSeedError)
import           Pos.Ssc.VerifyError      (SscVerifyError, sscIsCriticalError)
import           Pos.Util.Chrono          (NE, NewestFirst, OldestFirst)
import           Pos.Util.Util            (Some, inAssertMode, _neHead, _neLast)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Applies state changes to given var.
syncingStateWith
    :: TVar s
    -> StateT s (NamedPureLogger STM) a
    -> NamedPureLogger STM a
syncingStateWith var action = do
    oldV <- lift $ readTVar var
    (res, newV) <- runStateT action oldV
    lift $ writeTVar var newV
    return res

-- | Run something that reads 'SscLocalData' in 'MonadSscMem'.
-- 'MonadIO' is also needed to use stm.
sscRunLocalQuery
    :: forall ctx m a.
       (MonadSscMem ctx m, MonadIO m)
    => ReaderT SscLocalData m a -> m a
sscRunLocalQuery action = do
    localVar <- sscLocal <$> askSscMem
    ld <- atomically $ readTVar localVar
    runReaderT action ld

-- | Run STM transaction which modifies 'SscLocalData' and also can log.
sscRunLocalSTM
    :: forall ctx m a.
       (MonadSscMem ctx m, MonadIO m, WithLogger m)
    => StateT SscLocalData (NamedPureLogger STM) a -> m a
sscRunLocalSTM action = do
    localVar <- sscLocal <$> askSscMem
    launchNamedPureLog atomically $ syncingStateWith localVar action

-- | Run something that reads 'SscGlobalState' in 'MonadSscMem'.
-- 'MonadIO' is also needed to use stm.
sscRunGlobalQuery
    :: forall ctx m a.
       (MonadSscMem ctx m, MonadIO m)
    => ReaderT SscGlobalState m a -> m a
sscRunGlobalQuery action = do
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    runReaderT action gs

----------------------------------------------------------------------------
-- Seed calculation
----------------------------------------------------------------------------

-- | Calculate 'SharedSeed' for given epoch.
sscCalculateSeed
    :: forall ctx m.
       ( MonadSscMem ctx m
       , MonadDBRead m
       , SscGStateClass
       , MonadReader ctx m
       , HasLrcContext ctx
       , MonadIO m
       , WithLogger m )
    => EpochIndex
    -> m (Either SscSeedError SharedSeed)
sscCalculateSeed epoch = do
    -- We take richmen for the previous epoch because during N-th epoch we
    -- were using richmen for N-th epoch for everything – so, when we are
    -- calculating the seed for N+1-th epoch, we should still use data from
    -- N-th epoch.
    richmen <- getRichmenFromLrc "sscCalculateSeed" (epoch - 1)
    sscRunGlobalQuery $ sscCalculateSeedQ epoch richmen

----------------------------------------------------------------------------
-- Local Data
----------------------------------------------------------------------------

-- | Get 'SscPayload' for inclusion into main block with given 'SlotId'.
sscGetLocalPayload
    :: forall ctx m.
       (MonadIO m, MonadSscMem ctx m, SscLocalDataClass, WithLogger m)
    => SlotId -> m SscPayload
sscGetLocalPayload = sscRunLocalQuery . sscGetLocalPayloadQ

-- | Update local data to be valid for current global state.  This
-- function is assumed to be called after applying block and before
-- releasing lock on block application.
sscNormalize
    :: forall ctx m.
       ( MonadDBRead m
       , MonadGState m
       , MonadBlockDBGeneric (Some IsHeader) SscBlock () m
       , MonadSscMem ctx m
       , SscLocalDataClass
       , MonadReader ctx m
       , HasLrcContext ctx
       , WithLogger m
       , MonadIO m
       , Rand.MonadRandom m
       )
    => m ()
sscNormalize = do
    tipEpoch <- view epochIndexL <$> getTipHeaderGeneric @SscBlock
    richmenData <- getRichmenFromLrc "sscNormalize" tipEpoch
    bvd <- gsAdoptedBVData
    globalVar <- sscGlobal <$> askSscMem
    localVar <- sscLocal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    seed <- Rand.drgNew

    launchNamedPureLog atomically $
        syncingStateWith localVar $
        executeMonadBaseRandom seed $
        sscNormalizeU (tipEpoch, richmenData) bvd gs
  where
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed = hoist $ hoist (pure . fst . Rand.withDRG seed)

-- | Reset local data to empty state.  This function can be used when
-- we detect that something is really bad. In this case it makes sense
-- to remove all local data to be sure it's valid.
sscResetLocal ::
       forall ctx m.
       ( MonadDBRead m
       , MonadSscMem ctx m
       , SscLocalDataClass
       , MonadSlots ctx m
       , MonadIO m
       )
    => m ()
sscResetLocal = do
    emptyLD <- sscNewLocalData
    localVar <- sscLocal <$> askSscMem
    atomically $ writeTVar localVar emptyLD

----------------------------------------------------------------------------
-- GState
----------------------------------------------------------------------------

-- 'MonadIO' is needed only for 'TVar' (@gromak hopes).
-- 'MonadRandom' is needed for crypto (@neongreen hopes).
type SscGlobalVerifyMode ctx m =
    (MonadSscMem ctx m, SscGStateClass,
     MonadReader ctx m, HasLrcContext ctx,
     MonadDBRead m, MonadGState m, WithLogger m, MonadReporting ctx m,
     MonadIO m, Rand.MonadRandom m)

type SscGlobalApplyMode ctx m = SscGlobalVerifyMode ctx m

sscRunGlobalUpdate
    :: forall ctx m a.
       SscGlobalApplyMode ctx m
    => StateT SscGlobalState
       (NamedPureLogger (Rand.MonadPseudoRandom Rand.ChaChaDRG)) a
    -> m a
sscRunGlobalUpdate action = do
    globalVar <- sscGlobal <$> askSscMem
    seed <- Rand.drgNew
    launchNamedPureLog atomically $
        syncingStateWith globalVar $
        executeMonadBaseRandom seed action
  where
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed = hoist $ hoist (pure . fst . Rand.withDRG seed)

-- | Apply sequence of definitely valid blocks. Global state which is
-- result of application of these blocks can be optionally passed as
-- argument (it can be calculated in advance using 'sscVerifyBlocks').
sscApplyBlocks
    :: forall ctx m.
       SscGlobalApplyMode ctx m
    => OldestFirst NE SscBlock
    -> Maybe SscGlobalState
    -> m [SomeBatchOp]
sscApplyBlocks blocks (Just newState) = do
    inAssertMode $ do
        let hashes = map headerHash blocks
        expectedState <- sscVerifyValidBlocks blocks
        if | newState == expectedState -> pass
           | otherwise -> onUnexpectedVerify hashes
    sscApplyBlocksFinish newState
sscApplyBlocks blocks Nothing =
    sscApplyBlocksFinish =<< sscVerifyValidBlocks blocks

sscApplyBlocksFinish
    :: forall ctx m . SscGlobalApplyMode ctx m
    => SscGlobalState -> m [SomeBatchOp]
sscApplyBlocksFinish gs = do
    sscRunGlobalUpdate (put gs)
    inAssertMode $
        logDebug $
        sformat ("After applying blocks SSC global state is:\n"%build) gs
    pure $ sscGlobalStateToBatch gs

sscVerifyValidBlocks
    :: forall ctx m.
       SscGlobalApplyMode ctx m
    => OldestFirst NE SscBlock -> m SscGlobalState
sscVerifyValidBlocks blocks =
    sscVerifyBlocks blocks >>= \case
        Left e -> onVerifyFailedInApply hashes e
        Right newState -> return newState
  where
    hashes = map headerHash blocks

onVerifyFailedInApply
    :: forall m a.
       (WithLogger m, MonadThrow m)
    => OldestFirst NE HeaderHash -> SscVerifyError -> m a
onVerifyFailedInApply hashes e = assertionFailed msg
  where
    fmt =
        "sscApplyBlocks: verification of blocks "%listJson%" failed: "%build
    msg = sformat fmt hashes e

onUnexpectedVerify
    :: forall m a.
       (WithLogger m, MonadThrow m)
    => OldestFirst NE HeaderHash -> m a
onUnexpectedVerify hashes = assertionFailed msg
  where
    fmt =
        "sscApplyBlocks: verfication of blocks "%listJson%
        " returned unexpected state"
    msg = sformat fmt hashes

-- | Rollback application of given sequence of blocks. Bad things can
-- happen if these blocks haven't been applied before.
sscRollbackBlocks
    :: forall ctx m.
       SscGlobalApplyMode ctx m
    => NewestFirst NE SscBlock -> m [SomeBatchOp]
sscRollbackBlocks blocks = sscRunGlobalUpdate $ do
    sscRollbackU blocks
    sscGlobalStateToBatch <$> get

-- | Verify sequence of blocks and return global state which
-- corresponds to application of given blocks. If blocks are invalid,
-- this function will return 'Left' with appropriate error.
-- All blocks must be from the same epoch.
sscVerifyBlocks ::
       forall ctx m. SscGlobalVerifyMode ctx m
    => OldestFirst NE SscBlock
    -> m (Either SscVerifyError SscGlobalState)
sscVerifyBlocks blocks = do
    let epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    let lastEpoch = blocks ^. _Wrapped . _neLast . epochIndexL
    let differentEpochsMsg =
            sformat
                ("sscVerifyBlocks: different epochs ("%int%", "%int%")")
                epoch
                lastEpoch
    inAssertMode $ unless (epoch == lastEpoch) $
        assertionFailed differentEpochsMsg
    richmenSet <- getRichmenFromLrc "sscVerifyBlocks" epoch
    bvd <- gsAdoptedBVData
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    res <-
        runExceptT
            (execStateT (sscVerifyAndApplyBlocks richmenSet bvd blocks) gs)
    case res of
        Left e
            | sscIsCriticalError e ->
                reportError $ sformat ("Critical error in ssc: "%build) e
        _ -> pass
    return res

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

getRichmenFromLrc ::
       (MonadIO m, MonadDBRead m, MonadReader ctx m, HasLrcContext ctx)
    => Text
    -> EpochIndex
    -> m RichmenStakes
getRichmenFromLrc fname epoch =
    lrcActionOnEpochReason
        epoch
        (fname <> ": couldn't get SSC richmen")
        getRichmenSsc
