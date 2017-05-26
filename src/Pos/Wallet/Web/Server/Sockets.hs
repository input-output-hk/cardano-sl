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
       , initWSConnections
       , closeWSConnections
       , upgradeApplicationWS
       , notifyAll
       , runWalletWS
       ) where

import           Universum

import           Control.Concurrent.STM.TVar    (swapTVar)
import           Data.Aeson                     (encode)
import           Data.Default                   (Default (def))
import qualified Data.IntMap.Strict             as IM
import qualified Ether
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS

import           Pos.Aeson.ClientTypes          ()
import           Pos.Wallet.Web.ClientTypes     (NotifyEvent (ConnectionClosed, ConnectionOpened))
import           Pos.Wallet.Web.Error           (WalletError (..))


type WSConnection = WS.Connection
type ConnectionsVar = TVar ConnectionMap

initWSConnections :: MonadIO m => m ConnectionsVar
initWSConnections = newTVarIO def

closeWSConnection :: (MonadIO m, MonadThrow m) => ConnectionKey -> ConnectionsVar -> m ()
closeWSConnection key var = liftIO $ do
    maybeConn <- atomically $ deregisterConnection key var
    case maybeConn of
        Nothing   -> throwM $ InternalError "Attempted to close an unknown connection"
        Just conn -> WS.sendClose conn ConnectionClosed

closeWSConnections :: (MonadIO m, MonadThrow m) => ConnectionsVar -> m ()
closeWSConnections var = liftIO $ do
    conns <- atomically $ swapTVar var def
    for_ (listConnections conns) $ flip WS.sendClose ConnectionClosed

appendWSConnection :: ConnectionsVar -> WS.ServerApp
appendWSConnection var pending = do
    conn <- WS.acceptRequest pending
    key <- atomically $ registerConnection conn var
    sendWS conn ConnectionOpened
    WS.forkPingThread conn 30
    -- TODO: how to create a server that will handle all previous connections AND this one?
    forever (ignoreData conn) `finally` releaseResources key
  where
    ignoreData :: WSConnection -> IO Text
    ignoreData = WS.receiveData
    releaseResources :: (MonadIO m, MonadThrow m) => ConnectionKey -> m ()
    releaseResources key = closeWSConnection key var -- TODO: log

-- FIXME: we have no authentication and accept all incoming connections.
-- Possible solution: reject pending connection if WS handshake doesn't have valid auth session token.
upgradeApplicationWS :: ConnectionsVar -> Application -> Application
upgradeApplicationWS var = websocketsOr WS.defaultConnectionOptions (appendWSConnection var)

-- sendClose :: MonadIO m => ConnectionsVar -> NotifyEvent -> m ()
-- sendClose = send WS.sendClose

-- Sends notification msg to connected client. If there is no connection, notification msg will be ignored.
sendWS :: MonadIO m => WSConnection -> NotifyEvent -> m ()
sendWS = send WS.sendTextData

send :: MonadIO m => (WSConnection -> NotifyEvent -> IO ()) -> WSConnection -> NotifyEvent -> m ()
send f conn msg = liftIO $ f conn msg

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

notifyAll :: WebWalletSockets m => NotifyEvent -> m ()
notifyAll msg = do
  var <- getWalletWebSockets
  conns <- readTVarIO var
  for_ (listConnections conns) $ flip sendWS msg

------------------------------------
-- Implementation of ConnectionMap
------------------------------------

data IntMapWithUnusedKey v = IntMapWithUnusedKey
    { unusedKey :: Int
    , mapping   :: IntMap v
    }

instance Default (IntMapWithUnusedKey v) where
  def = IntMapWithUnusedKey 0 IM.empty

type ConnectionKey = IM.Key
type ConnectionMap = IntMapWithUnusedKey WSConnection

registerConnection :: WSConnection -> ConnectionsVar -> STM ConnectionKey
registerConnection conn var = do
    conns <- readTVar var
    let (key, newConns) = registerConnectionPure conn conns
    writeTVar var newConns
    pure key

deregisterConnection :: ConnectionKey -> ConnectionsVar -> STM (Maybe WSConnection)
deregisterConnection key var = do
    conns <- readTVar var
    case deregisterConnectionPure key conns of
        Nothing -> pure Nothing
        Just (conn, newConns) -> do
            writeTVar var newConns
            pure (Just conn)

registerConnectionPure :: WSConnection -> ConnectionMap -> (ConnectionKey, ConnectionMap)
registerConnectionPure conn conns =
  let newKey = unusedKey conns + 1 in
  (newKey, IntMapWithUnusedKey newKey (IM.insert newKey conn $ mapping conns))

deregisterConnectionPure :: ConnectionKey -> ConnectionMap -> Maybe (WSConnection, ConnectionMap)
deregisterConnectionPure key conns =
  case IM.lookup key (mapping conns) of
    Nothing -> Nothing
    Just conn -> Just (conn,
      IntMapWithUnusedKey (unusedKey conns) (IM.delete key $ mapping conns))

listConnections :: ConnectionMap -> [WSConnection]
listConnections = IM.elems . mapping
