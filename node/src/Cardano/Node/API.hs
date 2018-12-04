{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements the API defined in "Pos.Node.API".
module Cardano.Node.API where

import           Universum

import           Control.Concurrent.Async (concurrently_)
import           Control.Concurrent.STM (orElse, retry)
import           Control.Lens (lens, makeLensesWith, to)
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import           Data.Time.Units (toMicroseconds)
import           Network.HTTP.Types.Status (badRequest400)
import           Network.Wai (responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings,
                     setOnExceptionResponse)
import qualified Paths_cardano_sl_node as Paths
import           Servant

import           Ntp.Client (NtpConfiguration, NtpStatus (..),
                     ntpClientSettings, withNtpClient)
import           Ntp.Packet (NtpOffset)
import           Pos.Chain.Block (LastKnownHeader, LastKnownHeaderTag)
import           Pos.Chain.Ssc (SscContext)
import           Pos.Chain.Update (UpdateConfiguration, curSoftwareVersion)
import           Pos.Client.CLI.NodeOptions (NodeApiArgs (..))
import           Pos.Context (HasPrimaryKey (..), HasSscContext (..),
                     NodeContext (..))
import qualified Pos.Core as Core
import           Pos.Crypto (SecretKey)
import qualified Pos.DB.Block as DB
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.DB.Class as DB
import           Pos.DB.GState.Lock (Priority (..), StateLock,
                     withStateLockNoMetrics)
import qualified Pos.DB.Rocks as DB
import           Pos.DB.Txp.MemState (GenericTxpLocalData, TxpHolderTag)
import           Pos.Infra.Diffusion.Subscription.Status (ssMap)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import qualified Pos.Infra.Slotting.Util as Slotting
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Node.API as Node
import           Pos.Util (HasLens (..), HasLens')
import           Pos.Util.CompileInfo (CompileTimeInfo, ctiGitRevision)
import           Pos.Util.Lens (postfixLFields)
import           Pos.Util.Servant (APIResponse (..), JsendException (..),
                     UnknownError (..), applicationJson, single)
import           Pos.Web (serveImpl)
import qualified Pos.Web as Legacy

import Cardano.Node.API.Swagger (forkDocServer)

type NodeV1Api
    = "v1"
    :> ( Node.API
    :<|> Legacy.NodeApi
    )

nodeV1Api :: Proxy NodeV1Api
nodeV1Api = Proxy

data LegacyCtx = LegacyCtx
    { legacyCtxTxpLocalData
        :: !(GenericTxpLocalData ())
    , legacyCtxPrimaryKey
        :: !SecretKey
    , legacyCtxUpdateConfiguration
        :: !UpdateConfiguration
    , legacyCtxSscContext
        :: !SscContext
    , legacyCtxNodeDBs
        :: !DB.NodeDBs
    }

makeLensesWith postfixLFields ''LegacyCtx

instance HasPrimaryKey LegacyCtx where
    primaryKey = legacyCtxPrimaryKey_L

instance HasLens TxpHolderTag LegacyCtx (GenericTxpLocalData ()) where
    lensOf = legacyCtxTxpLocalData_L

instance HasLens DB.NodeDBs LegacyCtx DB.NodeDBs where
    lensOf = legacyCtxNodeDBs_L

instance HasLens UpdateConfiguration LegacyCtx UpdateConfiguration where
    lensOf = legacyCtxUpdateConfiguration_L

instance HasSscContext LegacyCtx where
    sscContext = legacyCtxSscContext_L

instance {-# OVERLAPPING #-} DB.MonadDBRead (ReaderT LegacyCtx IO) where
    dbGet = DB.dbGetDefault
    dbIterSource = DB.dbIterSourceDefault
    dbGetSerBlock = DB.dbGetSerBlockRealDefault
    dbGetSerUndo = DB.dbGetSerUndoRealDefault
    dbGetSerBlund = DB.dbGetSerBlundRealDefault

legacyNodeApi :: LegacyCtx -> Server Legacy.NodeApi
legacyNodeApi r =
    hoistServer
        (Proxy :: Proxy Legacy.NodeApi)
        (Handler . ExceptT . try . flip runReaderT r)
        Legacy.nodeServantHandlers

launchNodeServer
    :: NodeApiArgs
    -> NtpConfiguration
    -> NodeResources ()
    -> UpdateConfiguration
    -> CompileTimeInfo
    -> Diffusion IO
    -> IO ()
launchNodeServer
    params
    ntpConfig
    nodeResources
    updateConfiguration
    compileTimeInfo
    diffusion
  = do
    ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
    let legacyApi = legacyNodeApi LegacyCtx
            { legacyCtxTxpLocalData =
                nrTxpState nodeResources
            , legacyCtxPrimaryKey =
                view primaryKey . ncNodeParams $ nrContext nodeResources
            , legacyCtxUpdateConfiguration =
                updateConfiguration
            , legacyCtxSscContext =
                ncSscContext $ nrContext nodeResources
            , legacyCtxNodeDBs =
                nrDBs nodeResources
            }
    let app = serve nodeV1Api
            $ handlers
                diffusion
                ntpStatus
                (ncStateLock nodeCtx)
                (nrDBs nodeResources)
                (ncLastKnownHeader nodeCtx)
                slottingVarTimestamp
                slottingVar
                updateConfiguration
                compileTimeInfo
            :<|> legacyApi

    concurrently_
        (serveImpl
            (pure app)
            (BS8.unpack ipAddress)
            portNumber
            (do guard (not isDebug)
                nodeBackendTLSParams params)
            (Just exceptionResponse)
            Nothing -- TODO: Set a port callback for shutdown/IPC
            )
        (forkDocServer
            (Proxy @NodeV1Api)
            (curSoftwareVersion updateConfiguration)
            (BS8.unpack docAddress)
            docPort
            (do guard (not isDebug)
                nodeBackendTLSParams params)

        )
  where
    isDebug = nodeBackendDebugMode params
    exceptionResponse =
        setOnExceptionResponse handler defaultSettings
    handler exn =
        responseLBS badRequest400 [applicationJson]
            $ if isDebug
              then encode $ JsendException exn
              else encode $ UnknownError "An unknown error has occured."

    nodeCtx = nrContext nodeResources
    (slottingVarTimestamp, slottingVar) = ncSlottingVar nodeCtx
    (ipAddress, portNumber) = nodeBackendAddress params
    (docAddress, docPort) = nodeBackendDocAddress params

handlers
    :: Diffusion IO
    -> TVar NtpStatus
    -> StateLock
    -> DB.NodeDBs
    -> LastKnownHeader
    -> Core.Timestamp
    -> Core.SlottingVar
    -> UpdateConfiguration
    -> CompileTimeInfo
    -> ServerT Node.API Handler
handlers d t s n l ts sv uc ci =
    getNodeSettings ci uc ts sv
    :<|> getNodeInfo d t s n l
    :<|> applyUpdate
    :<|> postponeUpdate

getNodeSettings
    :: CompileTimeInfo
    -> UpdateConfiguration
    -> Core.Timestamp
    -> Core.SlottingVar
    -> Handler (APIResponse NodeSettings)
getNodeSettings compileInfo updateConfiguration timestamp slottingVar = do
    let ctx = SettingsCtx timestamp slottingVar
    slotDuration <-
        mkSlotDuration . fromIntegral <$>
            runReaderT Slotting.getNextEpochSlotDuration ctx

    pure $ single NodeSettings
        { setSlotDuration =
            slotDuration
        , setSoftwareInfo =
            V1 (curSoftwareVersion updateConfiguration)
        , setProjectVersion =
            V1 Paths.version
        , setGitRevision =
            Text.replace "\n" mempty (ctiGitRevision compileInfo)
        }

data SettingsCtx = SettingsCtx
    { settingsCtxTimestamp   :: Core.Timestamp
    , settingsCtxSlottingVar :: Core.SlottingVar
    }

instance Core.HasSlottingVar SettingsCtx where
    slottingTimestamp =
        lens settingsCtxTimestamp (\s t -> s { settingsCtxTimestamp = t })
    slottingVar =
        lens settingsCtxSlottingVar (\s t -> s { settingsCtxSlottingVar = t })

applyUpdate :: Handler NoContent
applyUpdate =
    throwError err500 { errBody = "This handler is not yet implemented." }

{-

More conversation from Slack about the Node API:

Matthias Benkort [1 day ago]
> If I am not mistaken, the only things these updates endpoint do are:
>
> - to store a flag when a new update is available (broadcast by the node to the wallet via a shared MVar)
>
> - to lookup / pop the last flag in db and restart the node
>
> Knowing that, restarting the node applies the last update anyway. So I wonder
> if we can't simply store that in plain memory. There's no need for persistence
> here, this is really just to remember a choice from daedalus. The typical flow
> is:
>
> - update is received by the node and broadcast to the wallet which stores it
>   in acid state.
> - every k seconds, Daedalus pulls the wallet to know about possible updates
> - when an update is available, a message is displayed to the user who has two
>   choices: "postpone" the update or apply it immediately.
> - The former removes the update marker from acid-state such that, there's no
>   "new" update event; the update will be applied anyway once Daedalus is
>   shutdown (better double-check with some frontend folks though)
> - The later removes it as well but restart the node immediately.
>
> And note that this does affect the node and creates actual coupling, so we
> ought to remove it, eventually.
>
> I truly believe acid-state or any form of persistent storage is no need
> however.

Duncan Coutts [1 day ago]
> I think it's right that no persistence is needed. Certainly in future the
> node's ledger state will simply report the current on-chain latest version of
> each application, so Daedalus or the wallet backend just needs to know it's
> own version and checks occasionally if the on-chain latest version is bigger.

-}

{-
-- from Cardano.Wallet.API.Internal.Handlers
applyUpdate :: PassiveWalletLayer IO -> Handler NoContent
applyUpdate w = liftIO (WalletLayer.applyUpdate w) >> return NoContent

-- from Cardano.WalletLayer.Kernel.hs
-- ...
        , applyUpdate          = Internal.applyUpdate         w
-- ...

-- from Cardano.Wallet.WalletLayer.Kernal.Internal
-- | Apply an update
--
-- NOTE (legacy): 'applyUpdate', "Pos.Wallet.Web.Methods.Misc".
--
-- The legacy implementation does two things:
--
-- 1. Remove the update from the wallet's list of updates
-- 2. Call 'applyLastUpdate' from 'MonadUpdates'
--
-- The latter is implemented in 'applyLastUpdateWebWallet', which literally just
-- a call to 'triggerShutdown'.
--
-- TODO: The other side of the story is 'launchNotifier', where the wallet
-- is /notified/ of updates.
applyUpdate :: MonadIO m => Kernel.PassiveWallet -> m ()
applyUpdate w = liftIO $ do
    update' (w ^. Kernel.wallets) $ RemoveNextUpdate
    Node.withNodeState (w ^. Kernel.walletNode) $ \_lock -> do
      doFail <- liftIO $ testLogFInject (w ^. Kernel.walletFInjects) FInjApplyUpdateNoExit
      unless doFail
        Node.triggerShutdown
-}

postponeUpdate :: Handler NoContent
postponeUpdate =
    throwError err500 { errBody = "This handler is not yet implemented." }

{-

-- from Cardano.Wallet.API.Internal.Handlers:
postponeUpdate :: PassiveWalletLayer IO -> Handler NoContent
postponeUpdate w = liftIO (WalletLayer.postponeUpdate w) >> return NoContent

-- from Cardano.Wallet.WalletLayer.Kernal.Internal
-- | Postpone update
--
-- NOTE (legacy): 'postponeUpdate', "Pos.Wallet.Web.Methods.Misc".
postponeUpdate :: MonadIO m => Kernel.PassiveWallet -> m ()
postponeUpdate w = update' (w ^. Kernel.wallets) $ RemoveNextUpdate

-}

getNodeInfo
    :: Diffusion IO
    -> TVar NtpStatus
    -> StateLock
    -> DB.NodeDBs
    -> LastKnownHeader
    -- endpoint parameters
    -> ForceNtpCheck
    -> Handler (APIResponse NodeInfo)
getNodeInfo diffusion ntpTvar stateLock nodeDBs lastknownHeader forceNtp = liftIO $ do
    single <$> do
        let r = InfoCtx
                { infoCtxStateLock = stateLock
                , infoCtxNodeDbs = nodeDBs
                , infoCtxLastKnownHeader = lastknownHeader
                }
        (mbNodeHeight, localHeight) <-
            runReaderT getNodeSyncProgress r
            -- view defaultSyncProgress impl

        localTimeInformation <-
            liftIO $ defaultGetNtpDrift ntpTvar forceNtp
            -- Node.getNtpDrift node forceNtp

        subscriptionStatus <-
            readTVarIO $ ssMap (subscriptionStates diffusion)

        pure NodeInfo
            { nfoSyncProgress =
                v1SyncPercentage mbNodeHeight localHeight
            , nfoBlockchainHeight =
                mkBlockchainHeight <$> mbNodeHeight
            , nfoLocalBlockchainHeight =
                mkBlockchainHeight localHeight
            , nfoLocalTimeInformation =
                localTimeInformation
            , nfoSubscriptionStatus =
                subscriptionStatus
            }

data InfoCtx = InfoCtx
    { infoCtxStateLock       :: StateLock
    , infoCtxNodeDbs         :: DB.NodeDBs
    , infoCtxLastKnownHeader :: LastKnownHeader
    }

instance HasLens DB.NodeDBs InfoCtx DB.NodeDBs where
    lensOf =
        lens infoCtxNodeDbs (\i s -> i { infoCtxNodeDbs = s })

instance HasLens StateLock InfoCtx StateLock where
    lensOf =
        lens infoCtxStateLock (\i s -> i { infoCtxStateLock = s })

instance HasLens LastKnownHeaderTag InfoCtx LastKnownHeader where
    lensOf =
        lens infoCtxLastKnownHeader (\i s -> i { infoCtxLastKnownHeader = s })

instance DB.MonadDBRead (ReaderT InfoCtx IO) where
    dbGet         = DB.dbGetDefault
    dbIterSource  = DB.dbIterSourceDefault
    dbGetSerBlock = DB.dbGetSerBlockRealDefault
    dbGetSerUndo  = DB.dbGetSerUndoRealDefault
    dbGetSerBlund = DB.dbGetSerBlundRealDefault


getNodeSyncProgress
    ::
    ( MonadIO m
    , MonadMask m
    , DB.MonadDBRead m
    , DB.MonadRealDB r m
    , HasLens' r StateLock
    , HasLens LastKnownHeaderTag r LastKnownHeader
    )
    => m (Maybe Core.BlockCount, Core.BlockCount)
getNodeSyncProgress = do
    (globalHeight, localHeight) <- withStateLockNoMetrics LowPriority $ \_ -> do
        -- We need to grab the localTip again as '_localTip' has type
        -- 'HeaderHash' but we cannot grab the difficulty out of it.
        headerRef <- view (lensOf @LastKnownHeaderTag)
        localTip  <- DB.getTipHeader
        mbHeader <- atomically $ readTVar headerRef `orElse` pure Nothing
        pure (view (Core.difficultyL . to Core.getChainDifficulty) <$> mbHeader
             ,view (Core.difficultyL . to Core.getChainDifficulty) localTip
             )
    return (max localHeight <$> globalHeight, localHeight)

-- | Computes the V1 'SyncPercentage' out of the global & local blockchain heights.
v1SyncPercentage :: Maybe Core.BlockCount -> Core.BlockCount -> SyncPercentage
v1SyncPercentage nodeHeight walletHeight =
    mkSyncPercentage $ case nodeHeight of
        Nothing ->
            0
        Just nd | walletHeight >= nd ->
            100
        Just nd ->
            floor @Double $
                ( fromIntegral walletHeight
                / max 1.0 (fromIntegral nd)
                ) * 100.0

-- | Get the difference between NTP time and local system time, nothing if the
-- NTP server couldn't be reached in the last 30min.
--
-- Note that one can force a new query to the NTP server in which case, it may
-- take up to 30s to resolve.
--
-- Copied from Cardano.Wallet.Kernel.NodeStateAdapter
defaultGetNtpDrift
    :: TVar NtpStatus
    -> ForceNtpCheck
    -> IO TimeInfo
defaultGetNtpDrift tvar ntpCheck = mkTimeInfo <$> case ntpCheck of
    ForceNtpCheck -> do
        forceNtpCheck
        getNtpOffset blockingLookupNtpOffset
    NoNtpCheck ->
        getNtpOffset nonBlockingLookupNtpOffset
  where
    forceNtpCheck =
        atomically $ writeTVar tvar NtpSyncPending

    getNtpOffset lookupNtpOffset =
        atomically (readTVar tvar >>= lookupNtpOffset)

    mkTimeInfo :: Maybe NtpOffset -> TimeInfo
    mkTimeInfo =
        TimeInfo . fmap (mkLocalTimeDifference . toMicroseconds)

    blockingLookupNtpOffset
        :: NtpStatus
        -> STM (Maybe NtpOffset)
    blockingLookupNtpOffset = \case
        NtpSyncPending     -> retry
        NtpDrift offset    -> pure (Just offset)
        NtpSyncUnavailable -> pure Nothing

    nonBlockingLookupNtpOffset
        :: NtpStatus
        -> STM (Maybe NtpOffset)
    nonBlockingLookupNtpOffset = \case
        NtpSyncPending     -> pure Nothing
        NtpDrift offset    -> pure (Just offset)
        NtpSyncUnavailable -> pure Nothing
