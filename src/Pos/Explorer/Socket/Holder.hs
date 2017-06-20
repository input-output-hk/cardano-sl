{-# LANGUAGE TemplateHaskell #-}

-- | `ExplorerSockets` monad.

module Pos.Explorer.Socket.Holder
       ( ExplorerSockets
       , MonadExplorerSockets (..)
       , runExplorerSockets
       , runNewExplorerSockets

       , ClientContext
       , ConnectionsState
       , ConnectionsVar
       , mkClientContext
       , mkConnectionsState
       , withConnState
       , askingConnState

       , csAddressSubscribers
       , csBlocksSubscribers
       , csBlocksPageSubscribers
       , csBlocksOffSubscribers
       , csTxsSubscribers
       , csClients
       , ccAddress
       , ccBlockOff
       , ccConnection
       ) where

import qualified Control.Concurrent.STM   as STM
--import           Control.Concurrent.STM.TVar (TVar)
import           Control.Lens             (makeClassy)
import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader     (MonadReader)
import           Control.Monad.Trans      (MonadIO (liftIO))
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S
import           Network.EngineIO         (SocketId)
import           Network.SocketIO         (Socket)
import           Serokell.Util.Concurrent (modifyTVarS)
import           System.Wlog              (NamedPureLogger, WithLogger,
                                           launchNamedPureLog)

import           Pos.Types                (Address)
import           Universum

data ClientContext = ClientContext
    { _ccAddress    :: !(Maybe Address)
    , _ccBlockOff   :: !(Maybe Word)
    , _ccConnection :: !Socket
    }

mkClientContext :: Socket -> ClientContext
mkClientContext = ClientContext Nothing Nothing

makeClassy ''ClientContext

data ConnectionsState = ConnectionsState
    { -- | Active sessions
      _csClients               :: !(M.Map SocketId ClientContext)
      -- | Sessions subscribed to given address.
    , _csAddressSubscribers    :: !(M.Map Address (S.Set SocketId))
      -- | Sessions subscribed to notifications about new blocks.
    , _csBlocksSubscribers     :: !(S.Set SocketId)
      -- | Sessions subscribed to notifications about new blocks with offset.
    , _csBlocksPageSubscribers :: !(S.Set SocketId)
      -- | Sessions subscribed to notifications about last page.
    , _csBlocksOffSubscribers  :: !(M.Map Word (S.Set SocketId))
      -- | Sessions subscribed to notifications about new transactions.
    , _csTxsSubscribers        :: !(S.Set SocketId)
    }

makeClassy ''ConnectionsState

type ConnectionsVar = TVar ConnectionsState

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csClients = mempty
    , _csAddressSubscribers = mempty
    , _csBlocksSubscribers = mempty
    , _csBlocksPageSubscribers = mempty
    , _csBlocksOffSubscribers = mempty
    , _csTxsSubscribers = mempty
    }

withConnState
    :: (MonadIO m, WithLogger m)
    => ConnectionsVar
    -> NamedPureLogger (StateT ConnectionsState STM) a
    -> m a
withConnState var = launchNamedPureLog $ liftIO . atomically . modifyTVarS var

askingConnState
    :: MonadIO m
    => ConnectionsVar
    -> ReaderT ConnectionsState m a
    -> m a
askingConnState var action = do
    v <- liftIO $ readTVarIO var
    runReaderT action v

-- TODO: not used, may be removed
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
