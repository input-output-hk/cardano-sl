{-# LANGUAGE TemplateHaskell #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Web.Sockets.App
       ( -- mkSocketIOApp
       ) where

import           Control.Concurrent.MVar (MVar, modifyMVar)
import           Control.Lens            (at, makeClassy, (%=), (+=), (.=), _Just)
import           Control.Monad           (filterM, forever, join, unless)
import           Control.Monad.Catch     (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader    (MonadReader)
import           Control.Monad.State     (MonadState, State, runState)
import           Control.Monad.Trans     (MonadIO (liftIO))
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Network.SocketIO        (Socket)

import           Pos.Types               (Address, ChainDifficulty)
import           Universum

type SessionId = Word

data ClientContext = ClientContext
    { _ccAddress    :: !(Maybe Address)
    , _ccHBlock     :: !(Maybe ChainDifficulty)
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
      -- | Sessions subscribed to notifications about new HBLocks.
    , _csHBlocksSubscribers :: !(S.Set SessionId)
    }

makeClassy ''ConnectionsState

startSession
    :: MonadState ConnectionsState m
    => Socket -> m SessionId
startSession conn = do
    i <- use csCounter
    csCounter += 1
    let cc = mkClientContext conn
    i <$ (csClients . at i .= Just cc)

finishSession :: MonadState ConnectionsState m => SessionId -> m ()
finishSession i = whenJustM (use $ csClients . at i) finishSessionDo
  where
    finishSessionDo _ = do
        csClients . at i .= Nothing
        unsubscribeHBlocks i
        unsubscribeAddr i

setClientAddress
    :: MonadState ConnectionsState m
    => SessionId -> Maybe Address -> m ()
setClientAddress sessId addr = do
    unsubscribeAddr sessId
    csClients . at sessId . _Just . ccAddress .= addr
    whenJust addr $ subscribeAddr sessId

setClientHBlock
    :: MonadState ConnectionsState m
    => SessionId -> Maybe ChainDifficulty -> m ()
setClientHBlock sessId pId = do
    csClients . at sessId . _Just . ccHBlock .= pId
    subscribeHBlocks sessId

subscribeAddr
    :: MonadState ConnectionsState m
    => SessionId -> Address -> m ()
subscribeAddr i addr =
    csAddressSubscribers . at addr %= Just .
    (maybe (S.singleton i) (S.insert i))

unsubscribeAddr
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeAddr i = do
    addr <- preuse $ csClients . at i . _Just . ccAddress
    whenJust (join addr) unsubscribeDo
  where
    unsubscribeDo a = csAddressSubscribers . at a %= fmap (S.delete i)

subscribeHBlocks
    :: MonadState ConnectionsState m
    => SessionId -> m ()
subscribeHBlocks i = csHBlocksSubscribers %= S.insert i

unsubscribeHBlocks
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeHBlocks i = csHBlocksSubscribers %= S.delete i

unsubscribeFully
    :: MonadState ConnectionsState m
    => SessionId -> m ()
unsubscribeFully i = unsubscribeHBlocks i >> unsubscribeAddr i

type ConnectionsVar = MVar ConnectionsState

mkConnectionsState :: ConnectionsState
mkConnectionsState =
    ConnectionsState
    { _csCounter = 0
    , _csClients = mempty
    , _csAddressSubscribers = mempty
    , _csHBlocksSubscribers = mempty
    }

modifyConnectionsStateDo
    :: MonadIO m
    => ConnectionsVar -> State ConnectionsState a -> m a
modifyConnectionsStateDo var st =
    liftIO $ modifyMVar var (pure . swap . runState st)
  where
    swap (a, b) = (b, a)

newtype SocketIOM a = SocketIOM
    { getServerMonad :: ReaderT ConnectionsVar IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader ConnectionsVar)
