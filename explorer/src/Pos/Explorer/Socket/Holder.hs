{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | `ExplorerSockets` monad.

module Pos.Explorer.Socket.Holder
       ( ExplorerSockets

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

import           Control.Lens     (makeClassy)
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import           Network.EngineIO (SocketId)
import           Network.SocketIO (Socket)
import           System.Wlog      (NamedPureLogger, WithLogger, dispatchEvents,
                                   getLoggerName, runNamedPureLogger, runPureLog,
                                   usingLoggerName)

import           Pos.Types        (Address)
import           Universum

data ClientContext = ClientContext
    { _ccAddress    :: !(Maybe Address)
    , _ccBlockOff   :: !(Maybe Word)
    , _ccConnection :: !Socket
    }

makeClassy ''ClientContext

mkClientContext :: Socket -> ClientContext
mkClientContext = ClientContext Nothing Nothing

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
    -> StateT ConnectionsState (NamedPureLogger STM) a
    -> m a
withConnState var m = do
    loggerName <- getLoggerName
    let runLog = usingLoggerName loggerName . runPureLog . runNamedPureLogger
    (a, logs) <- atomically $ do
        s <- readTVar var
        ((a, s'), logs) <- runLog $ runStateT m s
        (a, logs) <$ writeTVar var s'
    a <$ dispatchEvents logs

askingConnState
    :: MonadIO m
    => ConnectionsVar
    -> ExplorerSockets m a
    -> m a
askingConnState var action = do
    v <- readTVarIO var
    runReaderT action v

type ExplorerSockets = ReaderT ConnectionsState
