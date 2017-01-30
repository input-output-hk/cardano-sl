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
       , sscRunGlobalQuery

         -- * Seed calculation
       , sscCalculateSeed

         -- * Local Data
       , sscGetLocalPayload
       , sscNormalize
       , sscNormalizeRichmen

         -- * GState
       , sscApplyBlocks
       , sscRollbackBlocks
       , sscVerifyBlocks
       ) where

import           Control.Concurrent.STM  (readTVar, writeTVar)
import           Control.Lens            (_Wrapped)
import           Control.Monad.Except    (MonadError, runExceptT)
import           Control.Monad.State     (put)
import           Formatting              (build, sformat, (%))
import           Serokell.Util           (listJson)
import           System.Wlog             (LogEvent, LoggerName, LoggerNameBox, PureLogger,
                                          WithLogger, dispatchEvents, getLoggerName,
                                          logDebug, runPureLog, usingLoggerName)
import           Universum

import           Pos.Context             (WithNodeContext, lrcActionOnEpochReason)
import           Pos.DB                  (MonadDB)
import qualified Pos.DB.Lrc              as LrcDB
import           Pos.Exception           (reportFatalError)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage   (SscGStateClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Extra.Class     (MonadSscMem (askSscMem))
import           Pos.Ssc.Extra.Types     (SscState (sscGlobal, sscLocal))
import           Pos.Types               (Block, EpochIndex, HeaderHash, SharedSeed,
                                          SlotId, epochIndexL, headerHash)
import           Pos.Util                (NE, NewestFirst, OldestFirst, inAssertMode,
                                          _neHead)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

sscRunLocalQuery
    :: forall ssc m a.
       (MonadSscMem ssc m, MonadIO m)
    => ReaderT (SscLocalData ssc) m a -> m a
sscRunLocalQuery action = do
    localVar <- sscLocal <$> askSscMem
    ld <- atomically $ readTVar localVar
    runReaderT action ld

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

sscCalculateSeed
    :: forall ssc m.
       (MonadSscMem ssc m, SscGStateClass ssc, MonadIO m, WithLogger m)
    => EpochIndex -> m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed = sscRunGlobalQuery . sscCalculateSeedQ @ssc

----------------------------------------------------------------------------
-- Local Data
----------------------------------------------------------------------------

sscGetLocalPayload
    :: forall ssc m.
       (MonadIO m, MonadSscMem ssc m, SscLocalDataClass ssc)
    => SlotId -> m (Maybe (SscPayload ssc))
sscGetLocalPayload neededSlot = notImplemented

sscNormalize
    :: forall ssc m.
       (MonadIO m, MonadSscMem ssc m, SscLocalDataClass ssc)
    => m ()
sscNormalize = notImplemented

-- MonadDB is needed to get richmen.
sscNormalizeRichmen
    :: forall ssc m.
       (MonadDB ssc m, MonadSscMem ssc m, SscLocalDataClass ssc)
    => EpochIndex -> m ()
sscNormalizeRichmen = notImplemented

----------------------------------------------------------------------------
-- GState
----------------------------------------------------------------------------

-- 'MonadIO' (part of 'MonadDB')  is needed only for 'TVar'.
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
    convertRes ((res, events), st) = (res, st, events)

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
    sscRunGlobalUpdate $ put newState
sscApplyBlocks blocks Nothing =
    sscRunGlobalUpdate . put =<< sscVerifyValidBlocks blocks

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
    :: forall ssc m a.
       (WithLogger m, MonadThrow m)
    => OldestFirst NE HeaderHash -> m a
onUnexpectedVerify hashes = reportFatalError msg
  where
    fmt =
        "sscApplyBlocks: verfication of blocks "%listJson%
        " returned unexpected state"
    msg = sformat fmt hashes

sscRollbackBlocks
    :: forall ssc m.
       SscGlobalApplyMode ssc m
    => NewestFirst NE (Block ssc) -> m ()
sscRollbackBlocks = sscRunGlobalUpdate . sscRollbackU

sscVerifyBlocks
    :: forall ssc m.
       SscGlobalVerifyMode ssc m
    => OldestFirst NE (Block ssc) -> m (SscGlobalState ssc)
sscVerifyBlocks blocks = do
    let epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenSet <-
        lrcActionOnEpochReason
            epoch
            "couldn't get SSC richmen"
            LrcDB.getRichmenSsc
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    execStateT (sscVerifyAndApplyBlocks richmenSet blocks) gs
