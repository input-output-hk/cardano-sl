{-# LANGUAGE TemplateHaskell #-}

-- | Server launcher

module Pos.Explorer.Web.Sockets.App
       ( socketIOApp
       , test
       ) where

import           Control.Lens                       ((<<.=))
import           Data.ByteString                    (ByteString)
import qualified Data.Set                           as S
import           Data.Time.Units                    (Millisecond)
import           Mockable                           (Fork, Mockable)
import           Network.EngineIO                   (SocketId)
import           Network.EngineIO.Snap              (snapAPI)
import           Network.SocketIO                   (RoutingTable, Socket,
                                                     appendDisconnectHandler, initialize,
                                                     socketId)
import qualified Pos.DB                             as DB
import           Pos.Ssc.Class                      (SscHelpersClass)
import           Snap.Core                          (Response, route)
import           Snap.Http.Server                   (quickHttpServe)
import           System.Wlog                        (LoggerName, LoggerNameBox,
                                                     WithLogger, getLoggerName,
                                                     usingLoggerName)
import           Universum                          hiding (on)

import           Data.Map                           as M
import           Pos.Explorer.Web.Sockets.Holder    (ConnectionsState, ConnectionsVar,
                                                     mkConnectionsState,
                                                     modifyConnectionsStateDo,
                                                     readConnectionsStateDo)
import           Pos.Explorer.Web.Sockets.Instances ()
import           Pos.Explorer.Web.Sockets.Methods   (ClientEvent (..), blockAddresses,
                                                     getBlocksFromTo,
                                                     notifyAddrSubscribers,
                                                     notifyAllAddrSubscribers,
                                                     notifyBlocksSubscribers,
                                                     setClientAddress, setClientBlock,
                                                     startSession, subscribeAddr,
                                                     subscribeBlocks, unsubscribeAddr,
                                                     unsubscribeBlocks, unsubscribeFully)
import           Pos.Explorer.Web.Sockets.Util      (forkAccompanion, on, on_,
                                                     runPeriodicallyUnless)
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
    :: (MonadIO m, MonadMask m, DB.MonadDB ssc m, WithLogger m,
        SscHelpersClass ssc)
    => ConnectionsVar -> m Bool -> m ()
periodicPollChanges connVar closed =
    runPeriodicallyUnless (500 :: Millisecond) closed Nothing $ do
        curBlock  <- DB.getTip
        mPrevBlock <- identity <<.= Just curBlock

        -- notify about addrs
        mBlocks <- fmap join $ forM mPrevBlock $ \prevBlock ->
            getBlocksFromTo curBlock prevBlock 10
        notifiedAddrs <- case mBlocks of
            Nothing     -> return False
            Just blocks -> do
                addrs <- S.toList . mconcat . fmap S.fromList <$>
                    mapM blockAddresses blocks
                forM_ addrs $ \addr ->
                    readConnectionsStateDo connVar $ notifyAddrSubscribers addr
                return True

        -- notify about blocks
        when (mPrevBlock /= Just curBlock) $
            readConnectionsStateDo connVar $ do
                notifyBlocksSubscribers
                unless notifiedAddrs notifyAllAddrSubscribers

        -- or just `hasSender` + `hasReceiver` + `getTxOut`

socketIOApp
    :: (MonadIO m, MonadMask m, DB.MonadDB ssc m, Mockable Fork m,
        WithLogger m, SscHelpersClass ssc)
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
