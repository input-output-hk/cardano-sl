{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

-- | Server launcher.

module Pos.Explorer.Socket.App
       ( NotifierSettings (..)
       , notifierApp
       , toConfig
       ) where

import           Universum hiding (on)

import qualified Control.Concurrent.STM as STM
import           Control.Lens ((<<.=))
import           Control.Monad.State.Class (MonadState (..))
import qualified Data.Set as S
import           Data.Time.Units (Millisecond)
import           Ether.TaggedTrans ()
import           Formatting (int, sformat, (%))
import qualified GHC.Exts as Exts
import           Network.EngineIO (SocketId)
import           Network.EngineIO.Wai (WaiMonad, toWaiApplication, waiAPI)
import           Network.HTTP.Types.Status (status404)
import           Network.SocketIO (RoutingTable, Socket, appendDisconnectHandler, initialize,
                                   socketId)
import           Network.Wai (Application, Middleware, Request, Response, pathInfo, responseLBS)
import           Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy, Origin, cors, corsOrigins,
                                              simpleCorsResourcePolicy)
import           Serokell.Util.Text (listJson)
import           System.Wlog (CanLog, HasLoggerName, LoggerName, NamedPureLogger, WithLogger,
                              askLoggerName, logDebug, logInfo, logWarning, modifyLoggerName,
                              usingLoggerName)

import           Pos.Block.Types (Blund)
import           Pos.Core (addressF, siEpoch)
import qualified Pos.GState as DB
import           Pos.Slotting (MonadSlots (getCurrentSlot))

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.ExplorerMode (ExplorerMode)
import           Pos.Explorer.Socket.Holder (ConnectionsState, ConnectionsVar, askingConnState,
                                             mkConnectionsState, withConnState)
import           Pos.Explorer.Socket.Methods (ClientEvent (..), ServerEvent (..), Subscription (..),
                                              finishSession, getBlockTxs, getBlundsFromTo,
                                              getTxInfo, notifyAddrSubscribers,
                                              notifyBlocksLastPageSubscribers,
                                              notifyEpochsLastPageSubscribers, notifyTxsSubscribers,
                                              startSession, subscribeAddr, subscribeBlocksLastPage,
                                              subscribeEpochsLastPage, subscribeTxs,
                                              unsubscribeAddr, unsubscribeBlocksLastPage,
                                              unsubscribeEpochsLastPage, unsubscribeTxs)
import           Pos.Explorer.Socket.Util (emitJSON, forkAccompanion, on, on_, regroupBySnd,
                                           runPeriodicallyUnless)
import           Pos.Explorer.Web.ClientTypes (cteId, tiToTxEntry)
import           Pos.Explorer.Web.Server (getMempoolTxs)


data NotifierSettings = NotifierSettings
    { nsPort :: Word16
    } deriving (Show)

-- TODO(ks): Add logging, currently it's missing.
toConfig :: NotifierSettings -> LoggerName -> Settings
toConfig NotifierSettings{..} _ =
   setPort (fromIntegral nsPort) defaultSettings

newtype NotifierLogger a = NotifierLogger { runNotifierLogger :: NamedPureLogger (StateT ConnectionsState STM) a }
                         deriving (Functor, Applicative, Monad, CanLog, HasLoggerName, MonadThrow)

instance MonadState ConnectionsState NotifierLogger where
    get = NotifierLogger $ lift get
    put newState = NotifierLogger $ lift (put newState)

notifierHandler
    :: (MonadState RoutingTable m, MonadReader Socket m, CanLog m, MonadIO m)
    => ConnectionsVar -> LoggerName -> m ()
notifierHandler connVar loggerName = do
    _ <- asHandler' startSession
    on  (Subscribe SubAddr)             $ asHandler  subscribeAddr
    on_ (Subscribe SubBlockLastPage)    $ asHandler_ subscribeBlocksLastPage
    on_ (Subscribe SubTx)               $ asHandler_ subscribeTxs
    on_ (Subscribe SubEpochsLastPage)   $ asHandler_ subscribeEpochsLastPage
    on_ (Unsubscribe SubAddr)           $ asHandler_ unsubscribeAddr
    on_ (Unsubscribe SubBlockLastPage)  $ asHandler_ unsubscribeBlocksLastPage
    on_ (Unsubscribe SubTx)             $ asHandler_ unsubscribeTxs
    on_ (Unsubscribe SubEpochsLastPage) $ asHandler_ unsubscribeEpochsLastPage

    on_ CallMe                         $ emitJSON CallYou empty
    appendDisconnectHandler . void     $ asHandler_ finishSession
 where
    -- handlers provide context for logging and `ConnectionsVar` changes
    asHandler
        :: (a -> SocketId -> NotifierLogger ())
        -> a
        -> ReaderT Socket IO ()
    asHandler f arg = inHandlerCtx . f arg . socketId =<< ask
    asHandler_ f    = inHandlerCtx . f     . socketId =<< ask
    asHandler' f    = inHandlerCtx . f                =<< ask

    inHandlerCtx
        :: (MonadIO m, CanLog m)
        => NotifierLogger a
        -> m ()
    inHandlerCtx =
        -- currently @NotifierError@s aren't caught
        void . usingLoggerName loggerName . withConnState connVar . runNotifierLogger

notifierServer
    :: (MonadIO m, WithLogger m, MonadCatch m, WithLogger m)
    => NotifierSettings
    -> ConnectionsVar
    -> m ()
notifierServer notifierSettings connVar = do

    loggerName <- askLoggerName

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
                                    { corsOrigins = Just (origins, True)
                                    }
        -- HTTP origins that are allowed in CORS requests.
        -- Add more resources to the following list if needed.
        origins :: [Origin]
        origins =
            [ "https://cardanoexplorer.com"
            , "https://explorer.iohkdev.io"
            , "http://cardano-explorer.cardano-mainnet.iohk.io"
            , "http://localhost:3100"
            ]

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
    :: forall ctx m.
       (ExplorerMode ctx m)
    => ConnectionsVar -> m Bool -> m ()
periodicPollChanges connVar closed =
    -- Runs every 5 seconds.
    runPeriodicallyUnless (5000 :: Millisecond) closed (Nothing, mempty) $ do
        curBlock   <- DB.getTip
        mempoolTxs <- lift $ S.fromList <$> getMempoolTxs @ctx

        mWasBlock     <- _1 <<.= Just curBlock
        wasMempoolTxs <- _2 <<.= mempoolTxs

        lift . askingConnState connVar $ do
            mNewBlunds :: Maybe [Blund] <-
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

            -- notify changes depending on new blocks
            unless (null newBlunds) $ do
                -- 1. last page of blocks
                notifyBlocksLastPageSubscribers
                -- 2. last page of epochs
                mSlotId <- lift $ getCurrentSlot @ctx
                whenJust mSlotId $ notifyEpochsLastPageSubscribers . siEpoch
                logDebug $ sformat ("Blockchain updated ("%int%" blocks)")
                    (length newBlunds)

            newBlockchainTxs <- lift $ concatForM newBlunds (getBlockTxs @ctx . fst)
            let newLocalTxs = S.toList $ mempoolTxs `S.difference` wasMempoolTxs

            let allTxs = newBlockchainTxs <> newLocalTxs
            let cTxEntries = map tiToTxEntry allTxs
            txInfos <- Exts.toList . regroupBySnd <$> lift (mapM (getTxInfo @ctx) allTxs)

            -- notify about transactions
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
    :: forall ctx m.
       (ExplorerMode ctx m)
    => NotifierSettings -> m ()
notifierApp settings = modifyLoggerName (<> "notifier.socket-io") $ do
    logInfo "Starting"
    connVar <- liftIO $ STM.newTVarIO mkConnectionsState
    forkAccompanion (periodicPollChanges connVar)
                    (notifierServer settings connVar)
