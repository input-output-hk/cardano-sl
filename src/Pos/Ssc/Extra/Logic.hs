{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Control.Monad.State     (put)
import           Formatting              (build, int, sformat, (%))
import           Serokell.Util           (listJson)
import           System.Wlog             (LogEvent, LoggerName, LoggerNameBox, PureLogger,
                                          WithLogger, dispatchEvents, getLoggerName,
                                          logDebug, logError, runPureLog, usingLoggerName)
import           Universum

import           Pos.Context             (WithNodeContext, lrcActionOnEpochReason)
import           Pos.DB                  (MonadDB, getTipBlockHeader)
import qualified Pos.DB.Lrc              as LrcDB
import           Pos.Exception           (reportFatalError)
import           Pos.Slotting.Class      (MonadSlots)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage   (SscGStateClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Extra.Class     (MonadSscMem (askSscMem))
import           Pos.Ssc.Extra.Types     (SscState (sscGlobal, sscLocal))
import           Pos.Types               (Block, EpochIndex, HeaderHash, SharedSeed,
                                          SlotId, epochIndexL, headerHash)
import           Pos.Util                (NE, NewestFirst, OldestFirst, inAssertMode,
                                          _neHead, _neLast)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

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
    => (LoggerNameBox (PureLogger (StateT (SscLocalData ssc) STM)) a) -> m a
sscRunLocalSTM action = do
    loggerName <- getLoggerName
    localVar <- sscLocal <$> askSscMem
    (res, events) <-
        atomically $ do
            oldLD <- readTVar localVar
            ((res, events), !newLD) <-
                flip runStateT oldLD . runPureLog . usingLoggerName loggerName $
                action
            (res, events) <$ writeTVar localVar newLD
    res <$ dispatchEvents events

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
       (MonadSscMem ssc m, SscGStateClass ssc, MonadIO m, WithLogger m)
    => EpochIndex ->  m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed = sscRunGlobalQuery . sscCalculateSeedQ @ssc

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
       ( MonadDB ssc m
       , MonadSscMem ssc m
       , SscLocalDataClass ssc
       , WithNodeContext ssc m
       , SscHelpersClass ssc
       , WithLogger m
       )
    => m ()
sscNormalize = do
    tipEpoch <- view epochIndexL <$> getTipBlockHeader
    richmenData <-
        lrcActionOnEpochReason
            tipEpoch
            "sscNormalize: couldn't get SSC richmen"
            LrcDB.getRichmenSsc
    globalVar <- sscGlobal <$> askSscMem
    localVar <- sscLocal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    loggerName <- getLoggerName
    let sscNormalizeDo :: STM [LogEvent]
        sscNormalizeDo = do
            oldLD <- readTVar localVar
            let (((), logEvents), !newLD) =
                    flip runState oldLD .
                    runPureLog . usingLoggerName loggerName $
                    sscNormalizeU @ssc tipEpoch richmenData gs
            logEvents <$ writeTVar localVar newLD
    logEvents <- atomically sscNormalizeDo
    dispatchEvents logEvents

-- | Reset local data to empty state.  This function can be used when
-- we detect that something is really bad. In this case it makes sense
-- to remove all local data to be sure it's valid.
sscResetLocal
    :: forall ssc m.
       (MonadDB ssc m, MonadSscMem ssc m, SscLocalDataClass ssc, MonadSlots m)
    => m ()
sscResetLocal = do
    emptyLD <- sscNewLocalData
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
    (MonadSscMem ssc m, SscGStateClass ssc, WithLogger m, MonadDB ssc m, WithNodeContext ssc m)
type SscGlobalVerifyMode ssc m =
    (MonadSscMem ssc m, SscGStateClass ssc, WithLogger m,
     MonadDB ssc m, MonadError (SscVerifyError ssc) m, WithNodeContext ssc m)

sscRunGlobalUpdatePure
    :: forall ssc a.
       LoggerName
    -> SscGlobalState ssc
    -> LoggerNameBox (PureLogger (State (SscGlobalState ssc))) a
    -> (a, SscGlobalState ssc, [LogEvent])
sscRunGlobalUpdatePure loggerName st =
    convertRes . flip runState st . runPureLog . usingLoggerName loggerName
  where
    convertRes :: ((a, [LogEvent]), SscGlobalState ssc)
               -> (a, SscGlobalState ssc, [LogEvent])
    convertRes ((res, events), newSt) = (res, newSt, events)

sscRunGlobalUpdate
    :: forall ssc m a.
       SscGlobalApplyMode ssc m
    => LoggerNameBox (PureLogger (State (SscGlobalState ssc))) a -> m a
sscRunGlobalUpdate action = do
    loggerName <- getLoggerName
    globalVar <- sscGlobal <$> askSscMem
    (res, events) <- atomically $ sscRunGlobalUpdateDo loggerName globalVar
    res <$ dispatchEvents events
  where
    sscRunGlobalUpdateDo loggerName globalVar = do
        oldState <- readTVar globalVar
        let (res, !newState, events) =
                sscRunGlobalUpdatePure @ssc loggerName oldState action
        (res, events) <$ writeTVar globalVar newState

-- | Apply sequence of definitely valid blocks. Global state which is
-- result of application of these blocks can be optionally passed as
-- argument (it can be calculated in advance using 'sscVerifyBlocks').
sscApplyBlocks
    :: forall ssc m.
       SscGlobalApplyMode ssc m
    => OldestFirst NE (Block ssc) -> Maybe (SscGlobalState ssc) -> m ()
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
    :: SscGlobalApplyMode ssc m
    => SscGlobalState ssc -> m ()
sscApplyBlocksFinish gs = do
    sscRunGlobalUpdate (put gs)
    inAssertMode $
        logDebug $
        sformat ("After applying blocks SSC global state is:\n"%build) gs

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
onVerifyFailedInApply hashes e = reportFatalError msg
  where
    fmt =
        "sscApplyBlocks: verification of blocks "%listJson%" failed: "%build
    msg = sformat fmt hashes e

onUnexpectedVerify
    :: forall m a.
       (WithLogger m, MonadThrow m)
    => OldestFirst NE HeaderHash -> m a
onUnexpectedVerify hashes = reportFatalError msg
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
    => NewestFirst NE (Block ssc) -> m ()
sscRollbackBlocks = sscRunGlobalUpdate . sscRollbackU

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
    inAssertMode $ unless (epoch == lastEpoch) $ do
        logError differentEpochsMsg
        panic differentEpochsMsg
    richmenSet <-
        lrcActionOnEpochReason
            epoch
            "couldn't get SSC richmen"
            LrcDB.getRichmenSsc
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    execStateT (sscVerifyAndApplyBlocks richmenSet blocks) gs
