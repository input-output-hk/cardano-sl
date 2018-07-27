{-# LANGUAGE RankNTypes #-}

-- | Utilities for manipulating in-memory SSC state.

-- TODO: the names for functions are pretty bad
module Pos.Ssc.Mem
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
import qualified Crypto.Random as Rand
import           System.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

import           Pos.Ssc.Types (SscGlobalState, SscLocalData, SscState,
                     sscGlobal, sscLocal)
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
    -> StateT s (NamedPureLogger STM) a
    -> NamedPureLogger STM a
syncingStateWith var action = do
    oldV <- lift $ readTVar var
    (res, newV) <- runStateT action oldV
    lift $ writeTVar var newV
    return res

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

type SscLocalQuery a =
    forall m . (MonadReader SscLocalData m, WithLogger m) => m a

type SscLocalUpdate a =
    forall m . (MonadState SscLocalData m, WithLogger m, Rand.MonadRandom m) => m a

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
       (MonadSscMem ctx m, MonadIO m, WithLogger m)
    => StateT SscLocalData (NamedPureLogger STM) a -> m a
sscRunLocalSTM action = do
    localVar <- sscLocal <$> askSscMem
    launchNamedPureLog atomically $ syncingStateWith localVar action

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

type SscGlobalQuery a =
    forall m . (MonadReader SscGlobalState m) => m a

type SscGlobalUpdate a =
    forall m . (MonadState SscGlobalState m, WithLogger m, Rand.MonadRandom m) => m a

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
    :: (MonadSscMem ctx m, Rand.MonadRandom m, WithLogger m, MonadIO m)
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
    executeMonadBaseRandom seed =
        hoist $ hoist (pure . fst . Rand.withDRG seed)
