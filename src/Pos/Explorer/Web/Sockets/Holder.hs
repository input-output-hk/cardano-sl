{-# LANGUAGE TemplateHaskell #-}

-- | `ExplorerSockets` monad.

module Pos.Explorer.Web.Sockets.Holder
       ( ExplorerSockets
       , MonadExplorerSockets (..)
       , runExplorerSockets
       , runNewExplorerSockets

       , ConnectionsState
       , ConnectionsVar
       , mkClientContext
       , mkConnectionsState
       , withConnState
       , askingConnState

       , csAddressSubscribers
       , csBlocksSubscribers
       , csClients
       , ccAddress
       , ccBlock
       , ccConnection
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Lens                (makeClassy)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.Trans         (MonadIO (liftIO))
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import           Network.EngineIO            (SocketId)
import           Network.SocketIO            (Socket)
import           Serokell.Util.Concurrent    (modifyTVarS)
import           System.Wlog                 (LoggerNameBox, PureLogger, WithLogger,
                                              dispatchEvents, getLoggerName, runPureLog,
                                              usingLoggerName)

import           Pos.Types                   (Address, ChainDifficulty)
import           Universum

data ClientContext = ClientContext
    { _ccAddress    :: !(Maybe Address)
    , _ccBlock      :: !(Maybe ChainDifficulty)
    , _ccConnection :: !Socket
    }

mkClientContext :: Socket -> ClientContext
mkClientContext = ClientContext Nothing Nothing

makeClassy ''ClientContext

data ConnectionsState = ConnectionsState
    { -- | Active sessions
      _csClients            :: !(M.Map SocketId ClientContext)
      -- | Sessions subscribed to given address.
    , _csAddressSubscribers :: !(M.Map Address (S.Set SocketId))
      -- | Sessions subscribed to notifications about new BLocks.
    , _csBlocksSubscribers  :: !(S.Set SocketId)
    }

makeClassy ''ConnectionsState

type ConnectionsVar = TVar ConnectionsState

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csClients = mempty
    , _csAddressSubscribers = mempty
    , _csBlocksSubscribers = mempty
    }

withConnState
    :: (MonadIO m, WithLogger m)
    => ConnectionsVar
    -> LoggerNameBox (PureLogger (StateT ConnectionsState STM)) a
    -> m a
withConnState var action = do
    loggerName <- getLoggerName
    (res, logs) <- atomically $ modifyTVarS var $
        runPureLog $ usingLoggerName loggerName action
    dispatchEvents logs
    return res

askingConnState
    :: MonadIO m
    => ConnectionsVar
    -> ReaderT ConnectionsState m a
    -> m a
askingConnState var action = do
    v <- liftIO $ readTVarIO var
    runReaderT action v

newtype ExplorerSockets m a = ExplorerSockets
    { getExplorerSockets :: ReaderT ConnectionsVar m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch,
                MonadMask, MonadReader ConnectionsVar)

class Monad m => MonadExplorerSockets m where
    getSockets :: m ConnectionsVar

instance Monad m => MonadExplorerSockets (ExplorerSockets m) where
    getSockets = ExplorerSockets ask

runExplorerSockets :: ConnectionsVar -> ExplorerSockets m a -> m a
runExplorerSockets conn = flip runReaderT conn . getExplorerSockets

runNewExplorerSockets :: MonadIO m => ExplorerSockets m a -> m a
runNewExplorerSockets es = do
    conn <- liftIO $ STM.newTVarIO mkConnectionsState
    runExplorerSockets conn es
