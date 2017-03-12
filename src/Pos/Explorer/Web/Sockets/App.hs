{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server launcher

module Pos.Explorer.Web.Sockets.App
       ( NotifierSettings (..)
       , notifierApp
       ) where

import           Control.Concurrent.STM.TVar      (newTVarIO)
import           Control.Exception.Lifted         (try)
import           Control.Lens                     ((<<.=))
import           Control.Monad.Trans.Control      (MonadBaseControl)
import           Data.Aeson                       (Value)
import qualified Data.Set                         as S
import           Data.Time.Units                  (Millisecond)
import           Formatting                       (int, sformat, shown, stext, (%))
import           Mockable                         (Fork, Mockable)
import           Network.EngineIO                 (SocketId)
import           Network.EngineIO.Snap            (snapAPI)
import           Network.SocketIO                 (RoutingTable, Socket,
                                                   appendDisconnectHandler, initialize,
                                                   socketId)
import           Pos.DB.Class                     (MonadDB)
import qualified Pos.DB.GState                    as DB
import           Pos.Ssc.Class                    (SscHelpersClass)
import           Snap.Core                        (MonadSnap, route)
import qualified Snap.CORS                        as CORS
import           Snap.Http.Server                 (httpServe)
import qualified Snap.Internal.Http.Server.Config as Config
import           System.Wlog                      (CanLog, LoggerName, LoggerNameBox,
                                                   PureLogger, WithLogger, getLoggerName,
                                                   logDebug, logInfo, modifyLoggerName,
                                                   usingLoggerName)
import           Universum                        hiding (on)

import           Pos.Explorer.Aeson.ClientTypes   ()
import           Pos.Explorer.Web.ClientTypes     (CTxId)
import           Pos.Explorer.Web.Sockets.Error   (NotifierError)
import           Pos.Explorer.Web.Sockets.Holder  (ConnectionsState, ConnectionsVar,
                                                   askingConnState, mkConnectionsState,
                                                   withConnState)
import           Pos.Explorer.Web.Sockets.Methods (ClientEvent (..), ServerEvent (..),
                                                   blockAddresses, getBlocksFromTo,
                                                   notifyAddrSubscribers,
                                                   notifyAllAddrSubscribers,
                                                   notifyBlocksSubscribers,
                                                   setClientAddress, setClientBlock,
                                                   startSession, subscribeAddr,
                                                   subscribeBlocks, unsubscribeAddr,
                                                   unsubscribeBlocks, unsubscribeFully)
import           Pos.Explorer.Web.Sockets.Util    (emit, emitJSON, forkAccompanion, on,
                                                   on_, runPeriodicallyUnless)

data NotifierSettings = NotifierSettings
    { nsPort :: Word16
    }

toSnapConfig :: MonadSnap m => NotifierSettings -> Config.Config m ()
toSnapConfig NotifierSettings{..} = Config.defaultConfig
    { Config.port = Just $ fromIntegral nsPort
    }

notifierHandler
    :: (MonadState RoutingTable m, MonadReader Socket m, CanLog m, MonadIO m,
        MonadBaseControl IO m)
    => ConnectionsVar -> LoggerName -> m ()
notifierHandler connVar loggerName = do
    _                   <- asHandler' startSession
    on  SubscribeAddr    $ asHandler subscribeAddr
    on_ SubscribeBlock   $ asHandler_ subscribeBlocks
    on_ UnsubscribeAddr  $ asHandler_ unsubscribeAddr
    on_ UnsubscribeBlock $ asHandler_ unsubscribeBlocks
    on  SetClientAddress $ asHandler setClientAddress
    on  SetClientBlock   $ asHandler setClientBlock
    on_ CallMe           $ emitJSON CallYou empty
    on CallMeString      $ \(s :: Value) -> emit CallYouString s
    on CallMeTxId        $ \(txid :: CTxId) -> emit CallYouTxId txid
    appendDisconnectHandler . void $ asHandler_ unsubscribeFully
 where
    -- handlers provide context for logging and `ConnectionsVar` changes
    asHandler
        :: (a -> SocketId -> LoggerNameBox (PureLogger (StateT ConnectionsState STM)) b)
        -> a
        -> ReaderT Socket IO (Either NotifierError b)
    asHandler f arg = inHandlerCtx . f arg . socketId =<< ask
    asHandler_ f    = inHandlerCtx . f     . socketId =<< ask
    asHandler' f    = inHandlerCtx . f                =<< ask

    inHandlerCtx
        :: (MonadIO m, CanLog m, MonadBaseControl IO m)
        => LoggerNameBox (PureLogger (StateT ConnectionsState STM)) a
        -> m (Either NotifierError a)
    inHandlerCtx =
        try . usingLoggerName loggerName . withConnState connVar

notifierServer
    :: (MonadIO m, WithLogger m, MonadCatch m, WithLogger m)
    => NotifierSettings -> ConnectionsVar -> m ()
notifierServer settings connVar = do
    loggerName <- getLoggerName
    liftIO $ do
        handler <- liftIO $ initialize snapAPI $ notifierHandler connVar loggerName
        httpServe (toSnapConfig settings) $ CORS.applyCORS CORS.defaultOptions $
            route [("/socket.io", handler)]

periodicPollChanges
    :: forall ssc m.
       (MonadIO m, MonadMask m, MonadDB m, WithLogger m, SscHelpersClass ssc)
    => ConnectionsVar -> m Bool -> m ()
periodicPollChanges connVar closed =
    runPeriodicallyUnless (500 :: Millisecond) closed Nothing $ do
        curBlock  <- DB.getTip
        mWasBlock <- identity <<.= Just curBlock

        -- notify about addrs
        mBlocks <- fmap join $ forM mWasBlock $ \wasBlock ->
            getBlocksFromTo @ssc curBlock wasBlock
        notifiedAddrs <- case mBlocks of
            Nothing     -> return False
            Just blocks -> do
                addrs <- S.toList . mconcat . fmap S.fromList <$>
                    mapM blockAddresses blocks
                forM_ addrs $ \addr ->
                    askingConnState connVar $ notifyAddrSubscribers addr
                unless (null addrs) $
                    logDebug $ sformat ("Addresses updated: "%shown) addrs
                return True

        -- notify about blocks
        when (mWasBlock /= Just curBlock) $ do
            askingConnState connVar $ do
                notifyBlocksSubscribers
                unless notifiedAddrs notifyAllAddrSubscribers

            let blocksInfo = maybe "" (sformat $ " ("%int%" blocks)") $
                    length <$> mBlocks
            logDebug $ sformat ("Blockchain updated"%stext) blocksInfo

-- | Starts notification server. Kill current thread to stop it.
notifierApp
    :: forall ssc m.
       (MonadIO m, MonadMask m, MonadDB m, Mockable Fork m,
        WithLogger m, SscHelpersClass ssc)
    => NotifierSettings -> m ()
notifierApp settings = modifyLoggerName (<> "notifier") $ do
    logInfo "Starting"
    connVar <- liftIO $ newTVarIO mkConnectionsState
    forkAccompanion (periodicPollChanges @ssc connVar)
                    (notifierServer settings connVar)
