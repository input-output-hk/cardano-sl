{-# LANGUAGE TemplateHaskell #-}

-- | Server launcher

module Pos.Explorer.Web.Sockets.App
       ( socketIOApp
       , test
       ) where

import           Control.Lens                       ((<<.=))
import           Data.ByteString                    (ByteString)
import           Data.Time.Units                    (Millisecond)
import           Mockable                           (Fork, Mockable)
import           Network.EngineIO                   (SocketId)
import           Network.EngineIO.Snap              (snapAPI)
import           Network.SocketIO                   (RoutingTable, Socket,
                                                     appendDisconnectHandler, initialize,
                                                     socketId)
import           Snap.Core                          (Response, route)
import           Snap.Http.Server                   (quickHttpServe)
import           System.Wlog                        (LoggerName, LoggerNameBox,
                                                     WithLogger, getLoggerName,
                                                     usingLoggerName)
import           Universum                          hiding (on)

import qualified Pos.DB                             as DB
import           Pos.Explorer.Web.Sockets.Holder    (ConnectionsState, ConnectionsVar,
                                                     mkConnectionsState,
                                                     modifyConnectionsStateDo,
                                                     readConnectionsStateDo)
import           Pos.Explorer.Web.Sockets.Instances ()
import           Pos.Explorer.Web.Sockets.Methods   (ClientEvent (..),
                                                     notifyAllAddrSubscribers,
                                                     notifyBlocksSubscribers,
                                                     setClientAddress, setClientBlock,
                                                     startSession, subscribeAddr,
                                                     subscribeBlocks, unsubscribeAddr,
                                                     unsubscribeBlocks, unsubscribeFully)
import           Pos.Explorer.Web.Sockets.Util      (forkAccompanion, on, on_,
                                                     runPeriodicallyUnless)

import           Data.Map                           as M
import qualified Snap.Test                          as T

notifierHandler
    :: (MonadState RoutingTable m)
    => ConnectionsVar -> LoggerName -> m ()
notifierHandler connVar loggerName = do
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
        :: (MonadIO m, MonadMask m, MonadReader Socket m, MonadCatch m)
        => (SocketId -> a -> LoggerNameBox (StateT ConnectionsState m) b)
        -> a
        -> m b
    asHandler f arg = do
        sock <- ask
        modifyConnectionsStateDo connVar $ usingLoggerName loggerName $
            f (socketId sock) arg
    asHandler_ f = do
        sock <- ask
        modifyConnectionsStateDo connVar $ usingLoggerName loggerName $
            f (socketId sock)

notifierAddr :: ByteString
notifierAddr = "/notifier"

notifierServer
    :: (MonadIO m, WithLogger m, MonadCatch m, WithLogger m)
    => ConnectionsVar -> m ()
notifierServer connVar = do
    loggerName <- getLoggerName
    -- TODO: decide, whether to use /snap/, /yesod/, or smth else
    liftIO $ do
        handler <- liftIO $ initialize snapAPI $
            notifierHandler connVar loggerName
        quickHttpServe $ route [(notifierAddr, handler)]

periodicPollChanges
    :: (MonadIO m, MonadMask m, DB.MonadDB ssc m, WithLogger m)
    => ConnectionsVar -> m Bool -> m ()
periodicPollChanges connVar closed =
    runPeriodicallyUnless (500 :: Millisecond) closed Nothing $ do
        curBlock  <- Just <$> DB.getTip
        prevBlock <- identity <<.= curBlock
        when (curBlock /= prevBlock) $
            readConnectionsStateDo connVar $ do
                notifyBlocksSubscribers
                notifyAllAddrSubscribers  -- TODO: replace with smth more smart

socketIOApp
    :: (MonadIO m, MonadMask m, DB.MonadDB ssc m, Mockable Fork m,
        WithLogger m)
    => m ()
socketIOApp = do
    connVar <- liftIO $ newMVar mkConnectionsState
    forkAccompanion (periodicPollChanges connVar)
                    (notifierServer connVar)

-- TODO: tmp
test :: MonadIO m => m Response
test = liftIO $ do
    connVar <- liftIO $ newMVar mkConnectionsState
    handler <- initialize snapAPI $ notifierHandler connVar "*test*"
    T.runHandler (T.get "/notifier" M.empty) handler
