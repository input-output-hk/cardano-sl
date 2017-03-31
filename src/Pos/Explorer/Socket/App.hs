{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server launcher

module Pos.Explorer.Socket.App
       ( NotifierSettings (..)
       , notifierApp
       ) where

import qualified Control.Concurrent.STM           as STM
import           Control.Lens                     ((<<.=))
import           Control.Monad.Trans.Control      (MonadBaseControl)
import           Data.Aeson                       (Value)
import           Data.Time.Units                  (Millisecond)
import           Formatting                       (build, int, sformat, (%))
import qualified GHC.Exts                         as Exts
import           Mockable                         (Fork, Mockable)
import           Network.EngineIO                 (SocketId)
import           Network.EngineIO.Snap            (snapAPI)
import           Network.SocketIO                 (RoutingTable, Socket,
                                                   appendDisconnectHandler, initialize,
                                                   socketId)
import           Pos.DB.Class                     (MonadDB)
import qualified Pos.DB.GState                    as DB
import           Pos.Slotting.Class               (MonadSlots)
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
import           Pos.Explorer.Socket.Holder       (ConnectionsState, ConnectionsVar,
                                                   askingConnState, mkConnectionsState,
                                                   withConnState)
import           Pos.Explorer.Socket.Methods      (ClientEvent (..), ServerEvent (..),
                                                   Subscription (..), finishSession,
                                                   getBlockInfo, getBlocksFromTo,
                                                   groupBlockInfo, notifyAddrSubscribers,
                                                   notifyBlocksSubscribers,
                                                   notifyTxsSubscribers, startSession,
                                                   subscribeAddr, subscribeBlocks,
                                                   subscribeTxs, unsubscribeAddr,
                                                   unsubscribeBlocks, unsubscribeTxs)
import           Pos.Explorer.Socket.Util         (emit, emitJSON, forkAccompanion, on,
                                                   on_, runPeriodicallyUnless)
import           Pos.Explorer.Web.ClientTypes     (CTxId)

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
    _ <- asHandler' startSession
    on  (Subscribe SubAddr)    $ asHandler subscribeAddr
    on_ (Subscribe SubBlock)   $ asHandler_ subscribeBlocks
    on_ (Subscribe SubTx   )   $ asHandler_ subscribeTxs
    on_ (Unsubscribe SubAddr)  $ asHandler_ unsubscribeAddr
    on_ (Unsubscribe SubBlock) $ asHandler_ unsubscribeBlocks
    on_ (Unsubscribe SubTx)    $ asHandler_ unsubscribeTxs
    on_ CallMe                 $ emitJSON CallYou empty
    on CallMeString            $ \(s :: Value) -> emit CallYouString s
    on CallMeTxId              $ \(txid :: CTxId) -> emit CallYouTxId txid
    appendDisconnectHandler . void $ asHandler_ finishSession
 where
    -- handlers provide context for logging and `ConnectionsVar` changes
    asHandler
        :: (a -> SocketId -> LoggerNameBox (PureLogger (StateT ConnectionsState STM)) ())
        -> a
        -> ReaderT Socket IO ()
    asHandler f arg = inHandlerCtx . f arg . socketId =<< ask
    asHandler_ f    = inHandlerCtx . f     . socketId =<< ask
    asHandler' f    = inHandlerCtx . f                =<< ask

    inHandlerCtx
        :: (MonadIO m, CanLog m, MonadBaseControl IO m)
        => LoggerNameBox (PureLogger (StateT ConnectionsState STM)) a
        -> m ()
    inHandlerCtx =
        -- currently @NotifierError@s aren't caught
        void . usingLoggerName loggerName . withConnState connVar

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
       (MonadIO m, MonadMask m, MonadDB m, WithLogger m, SscHelpersClass ssc,
        MonadSlots m)
    => ConnectionsVar -> m Bool -> m ()
periodicPollChanges connVar closed =
    runPeriodicallyUnless (500 :: Millisecond) closed Nothing $ do
        curBlock  <- DB.getTip
        mWasBlock <- identity <<.= Just curBlock

        when (mWasBlock /= Just curBlock) $ do
            whenJust mWasBlock $ \wasBlock -> do
                mBlocks <- getBlocksFromTo @ssc curBlock wasBlock
                case mBlocks of
                    Nothing     -> do
                        logDebug "Failed to fetch blocks from db"
                    Just blocks -> askingConnState connVar $ do
                        notify blocks
  where
    notify blocks = do
        blockInfos <- concat <$> forM blocks getBlockInfo
        let groupedBlockInfos = Exts.toList $ groupBlockInfo blockInfos

        forM_ groupedBlockInfos $ \(addr, cTxEntries) -> do
            notifyAddrSubscribers addr cTxEntries
            logDebug $ sformat ("Notified address "%build%" about "
                       %int%" transactions") addr (length cTxEntries)

        -- notify about blocks
        notifyBlocksSubscribers blocks
        logDebug $ sformat ("Blockchain updated ("%int%" blocks)")
                   (length blocks)

        -- notify about transactions
        notifyTxsSubscribers $ fst <$> blockInfos


-- | Starts notification server. Kill current thread to stop it.
notifierApp
    :: forall ssc m.
       (MonadIO m, MonadMask m, MonadDB m, Mockable Fork m,
        WithLogger m, MonadSlots m, SscHelpersClass ssc)
    => NotifierSettings -> m ()
notifierApp settings = modifyLoggerName (<> "notifier") $ do
    logInfo "Starting"
    connVar <- liftIO $ STM.newTVarIO mkConnectionsState
    forkAccompanion (periodicPollChanges @ssc connVar)
                    (notifierServer settings connVar)
