{-# LANGUAGE TypeFamilies #-}

-- | Module for websockets implementation of Daedalus API.
-- This implements unidirectional sockets from server to client.
-- Every message received from client will be ignored.

module Pos.Wallet.Web.Server.Sockets
       ( WebWalletSockets
       , MonadWalletWebSockets
       , getWalletWebSockets
       , ConnectionsVar
       , initWSConnections
       , closeWSConnections
       , upgradeApplicationWS
       , notifyAll
       ) where

import           Universum

import           Control.Concurrent.STM.TVar    (swapTVar)
import           Control.Monad.State.Strict     (MonadState (get, put))
import           Data.Aeson                     (encode)
import           Data.Default                   (Default (def))
import qualified Data.IntMap.Strict             as IM
import qualified Ether
import           Formatting                     (build, sformat, (%))
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import           Serokell.Util.Concurrent       (modifyTVarS)
import           System.Wlog                    (logError, logNotice, usingLoggerName)

import           Pos.Aeson.ClientTypes          ()
import           Pos.Wallet.Web.ClientTypes     (NotifyEvent (ConnectionClosed, ConnectionOpened))


type WSConnection = WS.Connection
type ConnectionsVar = TVar ConnectionSet

initWSConnections :: MonadIO m => m ConnectionsVar
initWSConnections = newTVarIO def

closeWSConnection :: MonadIO m => ConnectionTag -> ConnectionsVar -> m ()
closeWSConnection tag var = liftIO $ usingLoggerName "closeWSConnection" $ do
    maybeConn <- atomically $ deregisterConnection tag var
    case maybeConn of
        Nothing   ->
            logError $ sformat ("Attempted to close an unknown connection with tag "%build) tag
        Just conn -> do
            liftIO $ WS.sendClose conn ConnectionClosed
            logNotice $ sformat ("Closed WS connection with tag "%build) tag

closeWSConnections :: MonadIO m => ConnectionsVar -> m ()
closeWSConnections var = liftIO $ do
    conns <- atomically $ swapTVar var def
    for_ (listConnections conns) $ flip WS.sendClose ConnectionClosed

appendWSConnection :: ConnectionsVar -> WS.ServerApp
appendWSConnection var pending = do
    conn <- WS.acceptRequest pending
    bracket (atomically $ registerConnection conn var) releaseResources $ \_ -> do
        sendWS conn ConnectionOpened
        WS.forkPingThread conn 30
        forever (ignoreData conn)
  where
    ignoreData :: WSConnection -> IO Text
    ignoreData = WS.receiveData
    releaseResources :: MonadIO m => ConnectionTag -> m ()
    releaseResources tag = closeWSConnection tag var

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

-- | MonadWalletWebSockets stands for monad which is able to get web wallet sockets
type MonadWalletWebSockets = Ether.MonadReader' ConnectionsVar

getWalletWebSockets :: MonadWalletWebSockets m => m ConnectionsVar
getWalletWebSockets = Ether.ask'

type WebWalletSockets m = (MonadWalletWebSockets m, MonadIO m)

notifyAll :: WebWalletSockets m => NotifyEvent -> m ()
notifyAll msg = do
  var <- getWalletWebSockets
  conns <- readTVarIO var
  for_ (listConnections conns) $ flip sendWS msg

------------------------------------
-- Implementation of ConnectionSet
------------------------------------

data TaggedSet v = TaggedSet
    { tsUnusedTag :: IM.Key
    , tsData      :: IntMap v
    }

instance Default (TaggedSet v) where
  def = TaggedSet 0 IM.empty

type ConnectionTag = IM.Key
type ConnectionSet = TaggedSet WSConnection

registerConnection :: WSConnection -> ConnectionsVar -> STM ConnectionTag
registerConnection conn var =
    modifyTVarS var $ state $ registerConnectionPure conn

deregisterConnection :: ConnectionTag -> ConnectionsVar -> STM (Maybe WSConnection)
deregisterConnection tag var = modifyTVarS var $ do
    conns <- get
    case deregisterConnectionPure tag conns of
        Nothing -> pure Nothing
        Just (conn, newConns) -> do
            put newConns
            pure (Just conn)

registerConnectionPure :: WSConnection -> ConnectionSet -> (ConnectionTag, ConnectionSet)
registerConnectionPure conn conns =
  let newKey = tsUnusedTag conns + 1 in
  (newKey, TaggedSet newKey (IM.insert newKey conn $ tsData conns))

deregisterConnectionPure :: ConnectionTag -> ConnectionSet -> Maybe (WSConnection, ConnectionSet)
deregisterConnectionPure tag conns = do  -- Maybe monad
  conn <- IM.lookup tag (tsData conns)
  pure (conn, TaggedSet (tsUnusedTag conns) (IM.delete tag $ tsData conns))

listConnections :: ConnectionSet -> [WSConnection]
listConnections = IM.elems . tsData
