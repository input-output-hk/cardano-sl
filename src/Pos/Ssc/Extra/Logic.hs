{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Concurrent.STM  (readTVar, writeTVar)
import           Control.Lens            (_Wrapped)
import           Control.Monad.Except    (MonadError, runExceptT)
import           Control.Monad.Morph     (generalize, hoist)
import           Control.Monad.State     (get, put)
import           Data.Tagged             (untag)
import qualified Ether
import           Formatting              (build, int, sformat, (%))
import           Serokell.Util           (listJson)
import           System.Wlog             (NamedPureLogger, WithLogger, launchNamedPureLog,
                                          logDebug)
import           Universum

import           Pos.Context             (lrcActionOnEpochReason)
import           Pos.DB                  (MonadDB, SomeBatchOp)
import           Pos.DB.DB               (getTipBlockHeader)
import           Pos.Exception           (assertionFailed)
import           Pos.Lrc.Context         (LrcContext)
import qualified Pos.Lrc.DB              as LrcDB
import           Pos.Lrc.Types           (RichmenStake)
import           Pos.Slotting.Class      (MonadSlots)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage   (SscGStateClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Extra.Class     (MonadSscMem, askSscMem)
import           Pos.Ssc.Extra.Types     (SscState (sscGlobal, sscLocal))
import           Pos.Types               (Block, EpochIndex, HeaderHash, SharedSeed,
                                          SlotId, epochIndexL, headerHash)
import           Pos.Util                (inAssertMode, _neHead, _neLast)
import           Pos.Util.Chrono         (NE, NewestFirst, OldestFirst)

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
    :: forall ssc m a.
       (MonadSscMem ssc m, MonadIO m)
    => ReaderT (SscLocalData ssc) m a -> m a
sscRunLocalQuery action = do
    localVar <- sscLocal <$> askSscMem
    ld <- atomically $ readTVar localVar
    runReaderT action ld

-- | Run STM transaction which modifies 'SscLocalData' and also can log.
sscRunLocalSTM
    :: forall ssc m a.
       (MonadSscMem ssc m, MonadIO m, WithLogger m)
    => StateT (SscLocalData ssc) (NamedPureLogger STM) a -> m a
sscRunLocalSTM action = do
    localVar <- sscLocal <$> askSscMem
    launchNamedPureLog atomically $ syncingStateWith localVar action

-- | Run something that reads 'SscGlobalState' in 'MonadSscMem'.
-- 'MonadIO' is also needed to use stm.
sscRunGlobalQuery
    :: forall ssc m a.
       (MonadSscMem ssc m, MonadIO m)
    => ReaderT (SscGlobalState ssc) m a -> m a
sscRunGlobalQuery action = do
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    runReaderT action gs

----------------------------------------------------------------------------
-- Seed calculation
----------------------------------------------------------------------------

-- | Calculate 'SharedSeed' for given epoch.
sscCalculateSeed
    :: forall ssc m.
       ( MonadSscMem ssc m
       , MonadDB m
       , SscGStateClass ssc
       , Ether.MonadReader' LrcContext m
       , MonadIO m
       , WithLogger m )
    => EpochIndex
    -> m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed epoch = do
    -- We take richmen for the previous epoch because during N-th epoch we
    -- were using richmen for N-th epoch for everything – so, when we are
    -- calculating the seed for N+1-th epoch, we should still use data from
    -- N-th epoch.
    richmen <- getRichmenFromLrc "sscCalculateSeed" (epoch - 1)
    sscRunGlobalQuery $ sscCalculateSeedQ @ssc epoch richmen

----------------------------------------------------------------------------
-- Local Data
----------------------------------------------------------------------------

-- | Get 'SscPayload' for inclusion into main block with given 'SlotId'.
sscGetLocalPayload
    :: forall ssc m.
       (MonadIO m, MonadSscMem ssc m, SscLocalDataClass ssc, WithLogger m)
    => SlotId -> m (SscPayload ssc)
sscGetLocalPayload = sscRunLocalQuery . sscGetLocalPayloadQ @ssc

-- 'MonadDB' is needed to get richmen and tip header.
-- Node context is needed to get richmen.
-- | Update local data to be valid for current global state.  This
-- function is assumed to be called after applying block and before
-- releasing lock on block application.
sscNormalize
    :: forall ssc m.
       ( MonadDB m
       , MonadSscMem ssc m
       , SscLocalDataClass ssc
       , Ether.MonadReader' LrcContext m
       , SscHelpersClass ssc
       , WithLogger m
       )
    => m ()
sscNormalize = do
    tipEpoch <- view epochIndexL <$> getTipBlockHeader @ssc
    richmenData <- getRichmenFromLrc "sscNormalize" tipEpoch
    globalVar <- sscGlobal <$> askSscMem
    localVar <- sscLocal <$> askSscMem
    gs <- atomically $ readTVar globalVar

    launchNamedPureLog atomically $
        syncingStateWith localVar $
        sscNormalizeU @ssc tipEpoch richmenData gs

-- | Reset local data to empty state.  This function can be used when
-- we detect that something is really bad. In this case it makes sense
-- to remove all local data to be sure it's valid.
sscResetLocal
    :: forall ssc m.
       (MonadDB m, MonadSscMem ssc m, SscLocalDataClass ssc, MonadSlots m)
    => m ()
sscResetLocal = do
    emptyLD <- sscNewLocalData @ssc
    localVar <- sscLocal <$> askSscMem
    atomically $ writeTVar localVar emptyLD

----------------------------------------------------------------------------
-- GState
----------------------------------------------------------------------------

-- 'MonadIO' (part of 'MonadDB')  is needed only for 'TVar'.
-- 'MonadThrow' (part of 'MonadDB') is needed only in 'ApplyMode'.
-- 'MonadDB' is needed only to get richmen.
-- We can try to eliminate these constraints later.
type SscGlobalApplyMode ssc m =
    (MonadSscMem ssc m, SscGStateClass ssc, WithLogger m,
     MonadDB m, Ether.MonadReader' LrcContext m)
type SscGlobalVerifyMode ssc m =
    (MonadSscMem ssc m, SscGStateClass ssc, WithLogger m,
     MonadDB m, Ether.MonadReader' LrcContext m,
     MonadError (SscVerifyError ssc) m)

sscRunGlobalUpdate
    :: forall ssc m a.
       SscGlobalApplyMode ssc m
    => StateT (SscGlobalState ssc) (NamedPureLogger Identity) a -> m a
sscRunGlobalUpdate action = do
    globalVar <- sscGlobal <$> askSscMem
    launchNamedPureLog atomically $
        syncingStateWith globalVar $
        switchMonadBaseIdentityToSTM action
  where
    switchMonadBaseIdentityToSTM = hoist $ hoist generalize

-- | Apply sequence of definitely valid blocks. Global state which is
-- result of application of these blocks can be optionally passed as
-- argument (it can be calculated in advance using 'sscVerifyBlocks').
sscApplyBlocks
    :: forall ssc m.
       SscGlobalApplyMode ssc m
    => OldestFirst NE (Block ssc)
    -> Maybe (SscGlobalState ssc)
    -> m [SomeBatchOp]
sscApplyBlocks blocks (Just newState) = do
    inAssertMode $ do
        let hashes = headerHash <$> blocks
        expectedState <- sscVerifyValidBlocks blocks
        if | newState == expectedState -> pass
           | otherwise -> onUnexpectedVerify hashes
    sscApplyBlocksFinish newState
sscApplyBlocks blocks Nothing =
    sscApplyBlocksFinish =<< sscVerifyValidBlocks blocks

sscApplyBlocksFinish
    :: forall ssc m . SscGlobalApplyMode ssc m
    => SscGlobalState ssc -> m [SomeBatchOp]
sscApplyBlocksFinish gs = do
    sscRunGlobalUpdate (put gs)
    inAssertMode $
        logDebug $
        sformat ("After applying blocks SSC global state is:\n"%build) gs
    pure $ untag @ssc $ sscGlobalStateToBatch gs

sscVerifyValidBlocks
    :: forall ssc m.
       SscGlobalApplyMode ssc m
    => OldestFirst NE (Block ssc) -> m (SscGlobalState ssc)
sscVerifyValidBlocks blocks =
    runExceptT (sscVerifyBlocks @ssc blocks) >>= \case
        Left e -> onVerifyFailedInApply @ssc hashes e
        Right newState -> return newState
  where
    hashes = headerHash <$> blocks

onVerifyFailedInApply
    :: forall ssc m a.
       (Ssc ssc, WithLogger m, MonadThrow m)
    => OldestFirst NE HeaderHash -> SscVerifyError ssc -> m a
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
    :: forall ssc m.
       SscGlobalApplyMode ssc m
    => NewestFirst NE (Block ssc) -> m [SomeBatchOp]
sscRollbackBlocks blocks = sscRunGlobalUpdate $ do
    sscRollbackU blocks
    untag @ssc . sscGlobalStateToBatch <$> get

-- | Verify sequence of blocks and return global state which
-- corresponds to application of given blocks. If blocks are invalid,
-- this function will return it using 'MonadError' type class.
-- All blocks must be from the same epoch.
sscVerifyBlocks
    :: forall ssc m.
       SscGlobalVerifyMode ssc m
    => OldestFirst NE (Block ssc) -> m (SscGlobalState ssc)
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
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    execStateT (sscVerifyAndApplyBlocks richmenSet blocks) gs

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

getRichmenFromLrc
    :: (MonadDB m, Ether.MonadReader' LrcContext m)
    => Text -> EpochIndex -> m RichmenStake
getRichmenFromLrc fname epoch =
    lrcActionOnEpochReason
        epoch
        (fname <> ": couldn't get SSC richmen")
        LrcDB.getRichmenSsc
