{-# LANGUAGE RankNTypes #-}

-- | Utilities for manipulating in-memory SSC state.

-- TODO: the names for functions are pretty bad
module Pos.Chain.Ssc.Mem
       (
       -- * 'MonadSscMem'
         SscMemTag
       , MonadSscMem
       , askSscMem
       , syncingStateWith

       -- * Local state
       , SscLocalQuery
       , SscLocalUpdate
       , sscRunLocalQuery
       , sscRunLocalSTM

       -- * Global state
       , SscGlobalQuery
       , SscGlobalUpdate
       , sscRunGlobalQuery
       , sscRunGlobalUpdate
       ) where

import           Universum

import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Writer (WriterT (..))
import qualified Crypto.Random as Rand
import           Data.DList (DList)
import qualified Data.DList as DList

import           Pos.Chain.Ssc.Types (SscGlobalState, SscLocalData, SscState,
                     sscGlobal, sscLocal)
import           Pos.Util.Trace (traceWith)
import           Pos.Util.Trace.Named (LogItem, LogNamed, TraceNamed)
import           Pos.Util.Util (HasLens (..))

----------------------------------------------------------------------------
-- MonadSscMem
----------------------------------------------------------------------------

data SscMemTag

type MonadSscMem ctx m = (MonadReader ctx m, HasLens SscMemTag ctx SscState)

askSscMem :: MonadSscMem ctx m => m SscState
askSscMem = view (lensOf @SscMemTag)

-- | Applies state changes to given var.
syncingStateWith
    :: TVar s
    -> StateT s STM a
    -> STM a
syncingStateWith var action = do
    oldV <- readTVar var
    (res, newV) <- runStateT action oldV
    writeTVar var newV
    return res

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type SscLocalQuery a = forall m . Monad m =>
    TraceNamed m ->
    ReaderT SscLocalData m a

-- | logging item
type LoggedItem = LogNamed LogItem

type SscLocalUpdate a =
    WriterT (DList LoggedItem) (StateT SscLocalData (Rand.MonadPseudoRandom Rand.ChaChaDRG)) a

-- | Run something that reads 'SscLocalData' in 'MonadSscMem'.
-- 'MonadIO' is also needed to use stm.
sscRunLocalQuery
    :: forall ctx m a.
       (MonadSscMem ctx m, MonadIO m)
    => ReaderT SscLocalData m a -> m a
sscRunLocalQuery action = do
    localVar <- sscLocal <$> askSscMem
    ld <- readTVarIO localVar
    runReaderT action ld

-- | Run STM transaction which modifies 'SscLocalData' and also can log.
sscRunLocalSTM
    :: forall ctx m a.
       (MonadSscMem ctx m, MonadIO m)
    => TraceNamed m
    -> WriterT (DList LoggedItem) (StateT SscLocalData STM) a
    -> m a
sscRunLocalSTM logTrace action = do
    localVar <- sscLocal <$> askSscMem
    (a, logItems) <- atomically $ syncingStateWith localVar $ runWriterT action
    forM_ (DList.toList logItems) (traceWith logTrace)
    pure a

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type SscGlobalQuery a = forall m . Monad m =>
    TraceNamed m ->
    ReaderT SscGlobalState m a

type SscGlobalUpdate a =
    WriterT (DList LoggedItem) (StateT SscGlobalState (Rand.MonadPseudoRandom Rand.ChaChaDRG)) a

-- | Run something that reads 'SscGlobalState' in 'MonadSscMem'.
-- 'MonadIO' is also needed to use stm.
sscRunGlobalQuery
    :: (MonadSscMem ctx m, MonadIO m)
    => ReaderT SscGlobalState m a -> m a
sscRunGlobalQuery action = do
    globalVar <- sscGlobal <$> askSscMem
    gs <- readTVarIO globalVar
    runReaderT action gs

sscRunGlobalUpdate
    :: (MonadSscMem ctx m, Rand.MonadRandom m, MonadIO m)
    => TraceNamed m
    -> WriterT (DList LoggedItem) (StateT SscGlobalState (Rand.MonadPseudoRandom Rand.ChaChaDRG)) a
    -> m a
sscRunGlobalUpdate logTrace action = do
    globalVar <- sscGlobal <$> askSscMem
    seed <- Rand.drgNew
    (a, logItems) <- atomically $
        syncingStateWith globalVar $
        runWriterT $
        executeMonadBaseRandom seed action
    forM_ (DList.toList logItems) (traceWith logTrace)
    pure a
  where
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed =
        hoist $ hoist (pure . fst . Rand.withDRG seed)
