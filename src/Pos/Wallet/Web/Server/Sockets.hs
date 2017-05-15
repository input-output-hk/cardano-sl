{-# LANGUAGE TypeFamilies #-}

-- | Module for websockets implementation of Daedalus API.
-- This implements unidirectional sockets from server to client.
-- Every message received from client will be ignored.

module Pos.Wallet.Web.Server.Sockets
       ( WalletWebSockets
       , WebWalletSockets
       , MonadWalletWebSockets
       , getWalletWebSockets
       , ConnectionsVar
       , initWSConnection
       , closeWSConnection
       , upgradeApplicationWS
       , notify
       , runWalletWS
       ) where

import           Universum

import           Data.Aeson                     (encode)
import qualified Ether
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS

import           Pos.Aeson.ClientTypes          ()
import           Pos.Wallet.Web.ClientTypes     (NotifyEvent (ConnectionClosed, ConnectionOpened))

-- NODE: for now we are assuming only one client will be used. If there will be need for multiple clients we should extend and hold multiple connections here.
-- We might add multiple clients when we add user profiles but I am not sure if we are planning on supporting more at all.
type ConnectionsVar = TVar (Maybe WS.Connection)

initWSConnection :: MonadIO m => m ConnectionsVar
initWSConnection = newTVarIO Nothing

closeWSConnection :: MonadIO m => ConnectionsVar -> m ()
closeWSConnection var = liftIO $ do
    conn <- readTVarIO var
    atomically $ writeTVar var Nothing
    maybe mempty (flip WS.sendClose ConnectionClosed) conn

switchConnection :: ConnectionsVar -> WS.ServerApp
switchConnection var pending = do
    conn <- WS.acceptRequest pending
    closeWSConnection var
    atomically . writeTVar var $ Just conn
    sendWS var ConnectionOpened
    WS.forkPingThread conn 30
    forever (ignoreData conn) `finally` releaseResources
  where
    ignoreData :: WS.Connection -> IO Text
    ignoreData = WS.receiveData
    releaseResources = closeWSConnection var -- TODO: log

-- If there is a new pending ws connection, the old connection will be replaced with new one.
-- FIXME: this is not safe because someone can kick out previous ws connection. Authentication can solve this issue. Solution: reject pending connection if ws handshake doesn't have valid auth session token
upgradeApplicationWS :: ConnectionsVar -> Application -> Application
upgradeApplicationWS wsConn = websocketsOr WS.defaultConnectionOptions $ switchConnection wsConn

-- sendClose :: MonadIO m => ConnectionsVar -> NotifyEvent -> m ()
-- sendClose = send WS.sendClose

-- Sends notification msg to connected client. If there is no connection, notification msg will be ignored.
sendWS :: MonadIO m => ConnectionsVar -> NotifyEvent -> m ()
sendWS = send WS.sendTextData

send :: MonadIO m => (WS.Connection -> NotifyEvent -> IO ()) -> ConnectionsVar -> NotifyEvent -> m ()
send f connVar msg = liftIO $ maybe mempty (flip f msg) =<< readTVarIO connVar

instance WS.WebSocketsData NotifyEvent where
    fromLazyByteString _ = error "Attempt to deserialize NotifyEvent is illegal"
    toLazyByteString = encode

--------
-- API
--------

-- | Holder for web wallet data
type WalletWebSockets = Ether.ReaderT' ConnectionsVar

-- | MonadWalletWebSockets stands for monad which is able to get web wallet sockets
type MonadWalletWebSockets = Ether.MonadReader' ConnectionsVar

getWalletWebSockets :: MonadWalletWebSockets m => m ConnectionsVar
getWalletWebSockets = Ether.ask'

type WebWalletSockets m = (MonadWalletWebSockets m, MonadIO m)

runWalletWS :: ConnectionsVar -> WalletWebSockets m a -> m a
runWalletWS = flip Ether.runReaderT

notify :: WebWalletSockets m => NotifyEvent -> m ()
notify msg = getWalletWebSockets >>= flip sendWS msg
