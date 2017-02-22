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
       , modifyConnectionsStateDo
       , withConnectionsState

       , csAddressSubscribers
       , csBlocksSubscribers
       , csClients
       , ccAddress
       , ccBlock
       ) where

import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Lens            (makeClassy)
import           Control.Monad.Catch     (MonadCatch, MonadMask, MonadThrow, onException)
import           Control.Monad.Reader    (MonadReader)
import           Control.Monad.State     (runStateT)
import           Control.Monad.Trans     (MonadIO (liftIO))
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Network.EngineIO        (SocketId)
import           Network.SocketIO        (Socket)

import           Pos.Types               (Address, ChainDifficulty)
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

-- TODO: use TVar?
type ConnectionsVar = MVar ConnectionsState

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csClients = mempty
    , _csAddressSubscribers = mempty
    , _csBlocksSubscribers = mempty
    }

modifyConnectionsStateDo
    :: (MonadIO m, MonadMask m)
    => ConnectionsVar -> StateT ConnectionsState m a -> m a
modifyConnectionsStateDo var st =
    mask $ \restore -> do
        a      <- liftIO $ takeMVar var
        (b, a') <- restore (runStateT st a)
            `onException` liftIO (putMVar var a)
        liftIO $ putMVar var a'
        return b

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
    conn <- liftIO $ newMVar mkConnectionsState
    runExplorerSockets conn es

withConnectionsState
    :: (MonadIO m, MonadMask m, MonadExplorerSockets m)
    => StateT ConnectionsState m a -> m a
withConnectionsState modifier = do
    connState <- getSockets
    modifyConnectionsStateDo connState modifier
