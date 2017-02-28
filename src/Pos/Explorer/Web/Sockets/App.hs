{-# LANGUAGE TemplateHaskell #-}

-- | Server launcher

module Pos.Explorer.Web.Sockets.App
       ( NotifierSettings (..)
       , notifierApp
       , test
       ) where

import           Control.Lens                       ((<<.=))
import qualified Data.HashMap.Strict                as HM
import qualified Data.Set                           as S
import           Data.Time.Units                    (Millisecond)
import           Formatting                         (int, sformat, shown, stext, (%))
import           Mockable                           (Fork, Mockable)
import           Network.EngineIO                   (SocketId, srvGetQueryParams)
import           Network.EngineIO.Snap              (snapAPI)
import           Network.SocketIO                   (RoutingTable, Socket,
                                                     appendDisconnectHandler, initialize,
                                                     socketId)
import qualified Pos.DB                             as DB
import           Pos.Ssc.Class                      (SscHelpersClass)
import           Snap.Core                          (MonadSnap, Response, route)
import qualified Snap.CORS                          as CORS
import           Snap.Http.Server                   (httpServe)
import qualified Snap.Internal.Http.Server.Config   as Config
import           System.Wlog                        (LoggerName, LoggerNameBox,
                                                     WithLogger, getLoggerName, logDebug,
                                                     logInfo, modifyLoggerName,
                                                     usingLoggerName)
import           Universum                          hiding (on)

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

import qualified Data.Map                           as M
import qualified Snap.Test                          as T

data NotifierSettings = NotifierSettings
    { nsPort :: Word16
    }

toSnapConfig :: MonadSnap m => NotifierSettings -> Config.Config m ()
toSnapConfig NotifierSettings{..} = Config.defaultConfig
    { Config.port = Just $ fromIntegral nsPort
    }

notifierHandler
    :: (MonadState RoutingTable m)
    => ConnectionsVar -> LoggerName -> m ()
notifierHandler connVar loggerName = do
    on_ StartSession     $ asHandler' startSession
    on  SubscribeAddr    $ asHandler subscribeAddr
    on_ SubscribeBlock   $ asHandler_ subscribeBlocks
    on_ UnsubscribeAddr  $ asHandler_ unsubscribeAddr
    on_ UnsubscribeBlock $ asHandler_ unsubscribeBlocks
    on  SetClientAddress $ asHandler setClientAddress
    on  SetClientBlock   $ asHandler setClientBlock
    appendDisconnectHandler $ asHandler_ unsubscribeFully
 where
    -- handlers provide context for logging and `ConnectionsVar` changes
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
    asHandler' f = do
        modifyConnectionsStateDo connVar . usingLoggerName loggerName . f =<< ask


notifierServer
    :: (MonadIO m, WithLogger m, MonadCatch m, WithLogger m)
    => NotifierSettings -> ConnectionsVar -> m ()
notifierServer settings connVar = do
    loggerName <- getLoggerName
    liftIO $ do
        handler <- liftIO $ initialize api $
            withCORS $ notifierHandler connVar loggerName
        httpServe (toSnapConfig settings) $
            route [("/socket.io", handler)]
  where
    api = snapAPI { srvGetQueryParams =
        HM.insert "transport" ["polling"] <$> srvGetQueryParams snapAPI }
    withCORS = CORS.applyCORS CORS.defaultOptions

periodicPollChanges
    :: (MonadIO m, MonadMask m, DB.MonadDB ssc m, WithLogger m,
        SscHelpersClass ssc)
    => ConnectionsVar -> m Bool -> m ()
periodicPollChanges connVar closed =
    runPeriodicallyUnless (500 :: Millisecond) closed Nothing $ do
        curBlock  <- DB.getTip
        mWasBlock <- identity <<.= Just curBlock

        -- notify about addrs
        mBlocks <- fmap join $ forM mWasBlock $ \wasBlock ->
            getBlocksFromTo curBlock wasBlock 10
        notifiedAddrs <- case mBlocks of
            Nothing     -> return False
            Just blocks -> do
                addrs <- S.toList . mconcat . fmap S.fromList <$>
                    mapM blockAddresses blocks
                forM_ addrs $ \addr ->
                    readConnectionsStateDo connVar $ notifyAddrSubscribers addr
                unless (null addrs) $
                    logDebug $ sformat ("Addresses updated: "%shown) addrs
                return True

        -- notify about blocks
        when (mWasBlock /= Just curBlock) $ do
            readConnectionsStateDo connVar $ do
                notifyBlocksSubscribers
                unless notifiedAddrs notifyAllAddrSubscribers

            let blocksInfo = maybe "" (sformat $ " ("%int%" blocks)") $
                    length <$> mBlocks
            logDebug $ sformat ("Blockchain updated"%stext) blocksInfo

        -- or just `hasSender` + `hasReceiver` + `getTxOut`

-- | Starts notification server. Kill current thread to stop it.
notifierApp
    :: (MonadIO m, MonadMask m, DB.MonadDB ssc m, Mockable Fork m,
        WithLogger m, SscHelpersClass ssc)
    => NotifierSettings -> m ()
notifierApp settings = modifyLoggerName (<> "notifier") $ do
    logInfo "Starting"
    connVar <- liftIO $ newMVar mkConnectionsState
    forkAccompanion (periodicPollChanges connVar)
                    (notifierServer settings connVar)

-- TODO: tmp
test :: MonadIO m => m Response
test = liftIO $ do
    connVar <- liftIO $ newMVar mkConnectionsState
    handler <- initialize snapAPI $ notifierHandler connVar "*test*"
    T.runHandler (T.get "localhost:1234" M.empty) handler
