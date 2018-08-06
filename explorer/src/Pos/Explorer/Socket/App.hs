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
import           Network.SocketIO (RoutingTable, Socket,
                     appendDisconnectHandler, initialize, socketId)
import           Network.Wai (Application, Middleware, Request, Response,
                     pathInfo, responseLBS)
import           Network.Wai.Handler.Warp (Settings, defaultSettings,
                     runSettings, setPort)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy, Origin, cors,
                     corsOrigins, simpleCorsResourcePolicy)
import           Serokell.Util.Text (listJson)

import           Pos.Chain.Block (Blund)
import           Pos.Core (addressF, siEpoch)
import           Pos.Core.Conc (withAsync)
import qualified Pos.GState as DB
import           Pos.Infra.Slotting (MonadSlots (getCurrentSlot))
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebug,
                     logInfo, logWarning, natTrace)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.ExplorerMode (ExplorerMode)
import           Pos.Explorer.Socket.Holder (ConnectionsState, ConnectionsVar,
                     askingConnState, mkConnectionsState, withConnState)
import           Pos.Explorer.Socket.Methods (ClientEvent (..),
                     ServerEvent (..), Subscription (..), finishSession,
                     getBlockTxs, getBlundsFromTo, getTxInfo,
                     notifyAddrSubscribers, notifyBlocksLastPageSubscribers,
                     notifyEpochsLastPageSubscribers, notifyTxsSubscribers,
                     startSession, subscribeAddr, subscribeBlocksLastPage,
                     subscribeEpochsLastPage, subscribeTxs, unsubscribeAddr,
                     unsubscribeBlocksLastPage, unsubscribeEpochsLastPage,
                     unsubscribeTxs)
import           Pos.Explorer.Socket.Util (emitJSON, on, on_, regroupBySnd,
                     runPeriodically)
import           Pos.Explorer.Web.ClientTypes (cteId, tiToTxEntry)
import           Pos.Explorer.Web.Server (getMempoolTxs)


data NotifierSettings = NotifierSettings
    { nsPort :: Word16
    } deriving (Show)

-- TODO(ks): Add logging, currently it's missing.
toConfig :: NotifierSettings -> Settings
toConfig NotifierSettings{..} =
   setPort (fromIntegral nsPort) defaultSettings

newtype NotifierLogger a = NotifierLogger { runNotifierLogger :: (StateT ConnectionsState STM) a }
                         deriving (Functor, Applicative, Monad, MonadThrow)

instance MonadState ConnectionsState NotifierLogger where
    get = NotifierLogger $ get
    put newState = NotifierLogger $ put newState
instance MonadIO NotifierLogger where
    liftIO = liftIO    -- instance needed for compiler, but used?

notifierHandler
    :: ( MonadState RoutingTable m
       , MonadReader Socket m
       , MonadIO m
       )
    => TraceNamed IO -> ConnectionsVar -> m ()
notifierHandler logTrace connVar = do
    _ <- asHandler' (startSession logTrace)
    on  (Subscribe SubAddr)             $ asHandler  $ subscribeAddr logTrace
    on_ (Subscribe SubBlockLastPage)    $ asHandler_ $ subscribeBlocksLastPage logTrace
    on_ (Subscribe SubTx)               $ asHandler_ $ subscribeTxs logTrace
    on_ (Subscribe SubEpochsLastPage)   $ asHandler_ $ subscribeEpochsLastPage logTrace
    on_ (Unsubscribe SubAddr)           $ asHandler_ $ unsubscribeAddr logTrace
    on_ (Unsubscribe SubBlockLastPage)  $ asHandler_ $ unsubscribeBlocksLastPage logTrace
    on_ (Unsubscribe SubTx)             $ asHandler_ $ unsubscribeTxs logTrace
    on_ (Unsubscribe SubEpochsLastPage) $ asHandler_ $ unsubscribeEpochsLastPage logTrace

    on_ CallMe                         $ emitJSON CallYou empty
    appendDisconnectHandler . void     $ asHandler_ $ finishSession logTrace
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
        :: MonadIO m
        => NotifierLogger a
        -> m ()
    inHandlerCtx =
        -- currently @NotifierError@s aren't caught
        void . withConnState connVar . runNotifierLogger

notifierServer
    :: MonadIO m
    => TraceNamed IO
    -> NotifierSettings
    -> ConnectionsVar
    -> m ()
notifierServer logTrace notifierSettings connVar = do

    let settings :: Settings
        settings = toConfig notifierSettings

    liftIO $ do
        handler <- initialize waiAPI $ notifierHandler logTrace connVar
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
    => TraceNamed IO -> ConnectionsVar -> m ()
periodicPollChanges logTrace connVar =
    let logTrace' = natTrace liftIO logTrace
    in
    -- Runs every 5 seconds.
    runPeriodically (natTrace liftIO logTrace) (5000 :: Millisecond) (Nothing, mempty) $ do
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
                                logWarning logTrace' "Failed to fetch blocks from db"
                                return []
                            Just blocks -> return blocks
            let newBlunds = fromMaybe [] mNewBlunds

            -- notify changes depending on new blocks
            unless (null newBlunds) $ do
                -- 1. last page of blocks
                notifyBlocksLastPageSubscribers logTrace
                -- 2. last page of epochs
                mSlotId <- lift $ getCurrentSlot @ctx
                whenJust mSlotId $ (notifyEpochsLastPageSubscribers logTrace) . siEpoch
                logDebug logTrace' $ sformat ("Blockchain updated ("%int%" blocks)")
                    (length newBlunds)

            newBlockchainTxs <- lift $ concatForM newBlunds (getBlockTxs @ctx . fst)
            let newLocalTxs = S.toList $ mempoolTxs `S.difference` wasMempoolTxs

            let allTxs = newBlockchainTxs <> newLocalTxs
            let cTxEntries = map tiToTxEntry allTxs
            txInfos <- Exts.toList . regroupBySnd <$> lift (mapM (getTxInfo @ctx) allTxs)

            -- notify about transactions
            forM_ txInfos $ \(addr, cTxBriefs) -> do
                notifyAddrSubscribers @ctx logTrace addr cTxBriefs
                logDebug logTrace' $ sformat ("Notified address "%addressF%" about "
                           %int%" transactions") addr (length cTxBriefs)

            -- notify about transactions
            unless (null cTxEntries) $ do
                notifyTxsSubscribers @ctx logTrace cTxEntries
                logDebug logTrace' $ sformat ("Broadcasted transactions: "%listJson)
                           (cteId <$> cTxEntries)

-- | Starts notification server. Kill current thread to stop it.
notifierApp
    :: forall ctx m.
       ( ExplorerMode ctx m )
    => TraceNamed IO -> NotifierSettings -> m ()
notifierApp logTrace settings = do
    let logTrace' = appendName "notifier.socket-io" logTrace
    logInfo (natTrace liftIO logTrace') "Starting"
    connVar <- liftIO $ STM.newTVarIO mkConnectionsState
    withAsync (periodicPollChanges logTrace' connVar)
              (\_async -> notifierServer logTrace' settings connVar)
