{-# LANGUAGE TemplateHaskell #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Web.Sockets.Holder
       ( ExplorerSockets
       , MonadExplorerSockets (..)
       , runExplorerSockets
       , runNewExplorerSockets

       , SessionId
       , ConnectionsState
       , ConnectionsVar
       , mkClientContext
       , modifyConnectionsStateDo

       , csAddressSubscribers
       , csBlocksSubscribers
       , csCounter
       , csClients
       , ccAddress
       , ccBlock
       ) where

import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import           Control.Lens            (makeClassy)
import           Control.Monad.Catch     (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader    (MonadReader)
import           Control.Monad.Trans     (MonadIO (liftIO))
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Network.SocketIO        (Socket)

import           Pos.Types               (Address, ChainDifficulty)
import           Universum

type SessionId = Word

data ClientContext = ClientContext
    { _ccAddress    :: !(Maybe Address)
    , _ccBlock      :: !(Maybe ChainDifficulty)
    , _ccConnection :: !Socket
    }

mkClientContext :: Socket -> ClientContext
mkClientContext = ClientContext Nothing Nothing

makeClassy ''ClientContext

data ConnectionsState = ConnectionsState
    { -- | Conuter used to generate SessionIds.
      _csCounter            :: !Word
    , _csClients            :: !(M.Map SessionId ClientContext)
      -- | Sessions subscribed to given address.
    , _csAddressSubscribers :: !(M.Map Address (S.Set SessionId))
      -- | Sessions subscribed to notifications about new BLocks.
    , _csBlocksSubscribers  :: !(S.Set SessionId)
    }

makeClassy ''ConnectionsState

type ConnectionsVar = MVar ConnectionsState

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csCounter = 0
    , _csClients = mempty
    , _csAddressSubscribers = mempty
    , _csBlocksSubscribers = mempty
    }

modifyConnectionsStateDo
    :: MonadIO m
    => ConnectionsVar -> State ConnectionsState a -> m a
modifyConnectionsStateDo var st =
    liftIO $ modifyMVar var (pure . swap . runState st)

newtype ExplorerSockets m a = ExplorerSockets
    { getExplorerSockets :: ReaderT ConnectionsVar m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader ConnectionsVar)

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
