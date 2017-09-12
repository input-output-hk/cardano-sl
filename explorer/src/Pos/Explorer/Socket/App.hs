{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

-- | Server launcher

module Pos.Explorer.Socket.App
       ( NotifierSettings (..)
       , notifierApp
       , toConfig
       ) where

import           Universum                      hiding (on)

import qualified Control.Concurrent.STM         as STM
import           Control.Lens                   ((<<.=))
import qualified Data.Set                       as S
import           Data.Time.Units                (Millisecond)
import           Ether.TaggedTrans              ()
import           Formatting                     (int, sformat, (%))
import qualified GHC.Exts                       as Exts
import           Network.EngineIO               (SocketId)
import           Network.EngineIO.Wai           (WaiMonad, toWaiApplication, waiAPI)
import           Network.HTTP.Types.Status      (status404)
import           Network.SocketIO               (RoutingTable, Socket,
                                                 appendDisconnectHandler, initialize,
                                                 socketId)
import           Network.Wai                    (Application, Middleware, Request,
                                                 Response, pathInfo,
                                                 responseLBS)
import           Network.Wai.Handler.Warp       (Settings, defaultSettings, runSettings,
                                                 setPort)
import           Network.Wai.Middleware.Cors    (CorsResourcePolicy, cors, corsOrigins,
                                                 simpleCorsResourcePolicy)
import           System.Wlog                    (CanLog, LoggerName, NamedPureLogger,
                                                 WithLogger, getLoggerName, logDebug,
                                                 logInfo, logWarning, modifyLoggerName,
                                                 usingLoggerName)
import           Serokell.Util.Text             (listJson)

import           Pos.Block.Types                (Blund)
import           Pos.Core                       (addressF)
import qualified Pos.GState                     as DB
import           Pos.Ssc.Class                  (SscHelpersClass)
import           Pos.Ssc.GodTossing             (SscGodTossing)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Socket.Holder     (ConnectionsState, ConnectionsVar,
                                                 askingConnState, mkConnectionsState,
                                                 withConnState)
import           Pos.Explorer.Socket.Methods    (ClientEvent (..), ServerEvent (..),
                                                 Subscription (..), finishSession,
                                                 getBlockTxs, getBlundsFromTo, getTxInfo,
                                                 notifyAddrSubscribers,
                                                 notifyBlocksLastPageSubscribers,
                                                 notifyTxsSubscribers, startSession,
                                                 subscribeAddr, subscribeBlocksLastPage,
                                                 subscribeTxs, unsubscribeAddr,
                                                 unsubscribeBlocksLastPage,
                                                 unsubscribeTxs)
import           Pos.Explorer.Socket.Util       (emitJSON, forkAccompanion, on, on_,
                                                 regroupBySnd, runPeriodicallyUnless)
import           Pos.Explorer.Web.ClientTypes   (cteId, tiToTxEntry)
import           Pos.Explorer.Web.Server        (ExplorerMode, getMempoolTxs)


data NotifierSettings = NotifierSettings
    { nsPort :: Word16
    } deriving (Show)

-- TODO(ks): Add logging, currently it's missing.
toConfig :: NotifierSettings -> LoggerName -> Settings
toConfig NotifierSettings{..} _ =
   setPort (fromIntegral nsPort) defaultSettings

notifierHandler
    :: (MonadState RoutingTable m, MonadReader Socket m, CanLog m, MonadIO m)
    => ConnectionsVar -> LoggerName -> m ()
notifierHandler connVar loggerName = do
    _ <- asHandler' startSession
    on  (Subscribe SubAddr)            $ asHandler  subscribeAddr
    on_ (Subscribe SubBlockLastPage)   $ asHandler_ subscribeBlocksLastPage
    on_ (Subscribe SubTx)              $ asHandler_ subscribeTxs
    on_ (Unsubscribe SubAddr)          $ asHandler_ unsubscribeAddr
    on_ (Unsubscribe SubBlockLastPage) $ asHandler_ unsubscribeBlocksLastPage
    on_ (Unsubscribe SubTx)            $ asHandler_ unsubscribeTxs

    on_ CallMe                         $ emitJSON CallYou empty
    appendDisconnectHandler . void     $ asHandler_ finishSession
 where
    -- handlers provide context for logging and `ConnectionsVar` changes
    asHandler
        :: (a -> SocketId -> (NamedPureLogger $ StateT ConnectionsState STM) ())
        -> a
        -> ReaderT Socket IO ()
    asHandler f arg = inHandlerCtx . f arg . socketId =<< ask
    asHandler_ f    = inHandlerCtx . f     . socketId =<< ask
    asHandler' f    = inHandlerCtx . f                =<< ask

    inHandlerCtx
        :: (MonadIO m, CanLog m)
        => NamedPureLogger (StateT ConnectionsState STM) a
        -> m ()
    inHandlerCtx =
        -- currently @NotifierError@s aren't caught
        void . usingLoggerName loggerName . withConnState connVar

notifierServer
    :: (MonadIO m, WithLogger m, MonadCatch m, WithLogger m)
    => NotifierSettings
    -> ConnectionsVar
    -> m ()
notifierServer notifierSettings connVar = do

    loggerName <- getLoggerName

    let settings :: Settings
        settings = toConfig notifierSettings loggerName

    liftIO $ do
        handler <- initialize waiAPI $ notifierHandler connVar loggerName
        runSettings settings (addRequestCORSHeaders . app $ handler)

  where
    addRequestCORSHeaders :: Middleware
    addRequestCORSHeaders = cors addCORSHeader
      where
        addCORSHeader :: Request -> Maybe CorsResourcePolicy
        addCORSHeader _ = Just $ simpleCorsResourcePolicy
                                    -- HTTP origins that are allowed in CORS requests.
                                    -- Add more resources to the following list if needed.
                                    { corsOrigins = Just ([ "https://cardanoexplorer.com"
                                                          , "https://explorer.iohkdev.io"
                                                          , "http://localhost:3100"
                                                          ], True)
                                    }

    app :: WaiMonad () -> Application
    app sHandle req respond
        | (elem "socket.io" $ pathInfo req) = toWaiApplication sHandle req respond
        | otherwise = respond notFound

    -- A simple response when the socket.io route isn't matched. This is on a separate
    -- port then the main application.
    notFound :: Response
    notFound = responseLBS
        status404
        [("Content-Type", "text/plain")]
        "404 - Not Found"

periodicPollChanges
    :: forall ssc ctx m.
       (ExplorerMode ctx m, SscHelpersClass ssc)
    => ConnectionsVar -> m Bool -> m ()
periodicPollChanges connVar closed =
    -- Runs every 5 seconds.
    runPeriodicallyUnless (5000 :: Millisecond) closed (Nothing, mempty) $ do
        curBlock   <- DB.getTip
        mempoolTxs <- lift $ S.fromList <$> getMempoolTxs @ctx

        mWasBlock     <- _1 <<.= Just curBlock
        wasMempoolTxs <- _2 <<.= mempoolTxs

        lift . askingConnState connVar $ do
            mNewBlunds :: Maybe [Blund SscGodTossing] <-
                if mWasBlock == Just curBlock
                    then return Nothing
                    else forM mWasBlock $ \wasBlock -> do
                        mBlocks <- lift $ getBlundsFromTo @ctx curBlock wasBlock
                        case mBlocks of
                            Nothing     -> do
                                logWarning "Failed to fetch blocks from db"
                                return []
                            Just blocks -> return blocks
            let newBlunds = fromMaybe [] mNewBlunds

            -- notify about blocks and blocks with offset
            unless (null newBlunds) $ do
                notifyBlocksLastPageSubscribers
                logDebug $ sformat ("Blockchain updated ("%int%" blocks)")
                    (length newBlunds)

            newBlockchainTxs <- lift $ concat <$> forM newBlunds (getBlockTxs @SscGodTossing @ctx . fst)
            let newLocalTxs = S.toList $ mempoolTxs `S.difference` wasMempoolTxs

            let allTxs = newBlockchainTxs <> newLocalTxs
            let cTxEntries = map tiToTxEntry allTxs
            txInfos <- Exts.toList . regroupBySnd <$> lift (mapM (getTxInfo @ctx) allTxs)

            -- notify abuot transactions
            forM_ txInfos $ \(addr, cTxBriefs) -> do
                notifyAddrSubscribers @ctx addr cTxBriefs
                logDebug $ sformat ("Notified address "%addressF%" about "
                           %int%" transactions") addr (length cTxBriefs)

            -- notify about transactions
            unless (null cTxEntries) $ do
                notifyTxsSubscribers @ctx cTxEntries
                logDebug $ sformat ("Broadcasted transactions: "%listJson)
                           (cteId <$> cTxEntries)

-- | Starts notification server. Kill current thread to stop it.
notifierApp
    :: forall ssc ctx m.
       (ExplorerMode ctx m, SscHelpersClass ssc)
    => NotifierSettings -> m ()
notifierApp settings = modifyLoggerName (<> "notifier.socket-io") $ do
    logInfo "Starting"
    connVar <- liftIO $ STM.newTVarIO mkConnectionsState
    forkAccompanion (periodicPollChanges @ssc connVar)
                    (notifierServer settings connVar)
