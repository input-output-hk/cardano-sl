{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module for websockets implementation of Daedalus API.
-- This implements unidirectional sockets from server to client.
-- Every message received from client will be ignored.

module Pos.Wallet.Web.Sockets.Connection
       ( MonadWalletWebSockets
       , getWalletWebSockets
       , initWSConnections
       , closeWSConnections
       , upgradeApplicationWS
       , notifyAll
       ) where

import           Universum

import           Control.Concurrent.STM.TVar (swapTVar)
import           Data.Aeson (encode)
import           Data.Default (Default (def))
import           Formatting (build, sformat, (%))
import           Network.Wai (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import           System.Wlog (logError, logNotice, usingLoggerName)

import           Pos.Util.Util (HasLens (..), HasLens')
import           Pos.Wallet.Aeson ()
import qualified Pos.Wallet.Web.Sockets.ConnSet as CS
import           Pos.Wallet.Web.Sockets.Types (NotifyEvent (ConnectionClosed, ConnectionOpened),
                                               WSConnection)


initWSConnections :: MonadIO m => m CS.ConnectionsVar
initWSConnections = newTVarIO def

closeWSConnection :: MonadIO m => CS.ConnectionTag -> CS.ConnectionsVar -> m ()
closeWSConnection tag var = liftIO $ usingLoggerName "closeWSConnection" $ do
    maybeConn <- atomically $ CS.deregisterConnection tag var
    case maybeConn of
        Nothing   ->
            logError $ sformat ("Attempted to close an unknown connection with tag "%build) tag
        Just conn -> do
            liftIO $ WS.sendClose conn ConnectionClosed
            logNotice $ sformat ("Closed WS connection with tag "%build) tag

closeWSConnections :: MonadIO m => CS.ConnectionsVar -> m ()
closeWSConnections var = liftIO $ do
    conns <- atomically $ swapTVar var def
    for_ (CS.listConnections conns) $ flip WS.sendClose ConnectionClosed

appendWSConnection :: CS.ConnectionsVar -> WS.ServerApp
appendWSConnection var pending = do
    conn <- WS.acceptRequest pending
    bracket (atomically $ CS.registerConnection conn var) releaseResources $ \_ -> do
        sendWS conn ConnectionOpened
        WS.forkPingThread conn 30
        forever (ignoreData conn)
  where
    ignoreData :: WSConnection -> IO Text
    ignoreData = WS.receiveData
    releaseResources :: MonadIO m => CS.ConnectionTag -> m ()
    releaseResources tag = closeWSConnection tag var

-- FIXME: we have no authentication and accept all incoming connections.
-- Possible solution: reject pending connection if WS handshake doesn't have valid auth session token.
upgradeApplicationWS :: CS.ConnectionsVar -> Application -> Application
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
type MonadWalletWebSockets ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , HasLens' ctx CS.ConnectionsVar
    )

getWalletWebSockets :: MonadWalletWebSockets ctx m => m CS.ConnectionsVar
getWalletWebSockets = view (lensOf @CS.ConnectionsVar)

notifyAll :: MonadWalletWebSockets ctx m => NotifyEvent -> m ()
notifyAll msg = do
  var <- getWalletWebSockets
  conns <- readTVarIO var
  for_ (CS.listConnections conns) $ flip sendWS msg
