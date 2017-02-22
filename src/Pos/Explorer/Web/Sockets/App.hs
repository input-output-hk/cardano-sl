{-# LANGUAGE TemplateHaskell #-}

-- | Server launcher

module Pos.Explorer.Web.Sockets.App
       ( notifierApp
       , notifierHandler  -- TODO: to remove
       , test
       ) where

import           Control.Concurrent.MVar            (newMVar)
import           Data.ByteString                    (ByteString)
import           Network.EngineIO                   (SocketId)
import           Network.EngineIO.Snap              (snapAPI)
import           Network.SocketIO                   (RoutingTable, Socket,
                                                     appendDisconnectHandler, initialize,
                                                     socketId)
import           Snap.Core                          (Response, route)
import           Snap.Http.Server                   (quickHttpServe)
import           Universum                          hiding (on)

import           Pos.Explorer.Web.Sockets.Holder    (ConnectionsState, ConnectionsVar,
                                                     mkConnectionsState,
                                                     modifyConnectionsStateDo)
import           Pos.Explorer.Web.Sockets.Instances ()
import           Pos.Explorer.Web.Sockets.Methods   (ClientEvent (..), setClientAddress,
                                                     setClientBlock, startSession,
                                                     subscribeAddr, subscribeBlocks,
                                                     unsubscribeAddr, unsubscribeBlocks,
                                                     unsubscribeFully)
import           Pos.Explorer.Web.Sockets.Util      (on, on_)

import           Data.Map                           as M
import           Snap.Test

notifierAddr :: ByteString
notifierAddr = "/notifier"

notifierHandler
    :: MonadState RoutingTable m
    => ConnectionsVar -> m ()
notifierHandler connVar = do
    on_ StartSession     $
        modifyConnectionsStateDo connVar . startSession =<< ask
    on  SubscribeAddr    $ asHandler subscribeAddr
    on_ SubscribeBlock   $ asHandler_ subscribeBlocks
    on_ UnsubscribeAddr  $ asHandler_ unsubscribeAddr
    on_ UnsubscribeBlock $ asHandler_ unsubscribeBlocks
    on  SetClientAddress $ asHandler setClientAddress
    on  SetClientBlock   $ asHandler setClientBlock
    appendDisconnectHandler $ asHandler_ unsubscribeFully
 where
    asHandler
        :: (MonadIO m, MonadMask m, MonadReader Socket m)
        => (SocketId -> a -> StateT ConnectionsState m b) -> a -> m b
    asHandler f arg = do
        sock <- ask
        modifyConnectionsStateDo connVar $ f (socketId sock) arg
    asHandler_ f = do
        sock <- ask
        modifyConnectionsStateDo connVar $ f (socketId sock)

notifierApp :: MonadIO m => ConnectionsVar -> m ()
notifierApp connVar = liftIO $ do
    -- TODO: decide, whether to use /snap/, /yesod/, or smth else
    handler <- liftIO $ initialize snapAPI $ notifierHandler connVar
    quickHttpServe $ route [(notifierAddr, handler)]

-- TODO: tmp
test :: MonadIO m => m Response
test = liftIO $ do
    connVar <- liftIO $ newMVar mkConnectionsState
    handler <- initialize snapAPI $ notifierHandler connVar
    runHandler (get "/notifier" M.empty) handler
