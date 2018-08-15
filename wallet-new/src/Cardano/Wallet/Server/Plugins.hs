{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.Server.Plugins (
      Plugin
    , syncWalletWorker
    , acidCleanupWorker
    , conversation
    , legacyWalletBackend
    , walletBackend
    , walletDocumentation
    , resubmitterPlugin
    , notifierPlugin
    ) where

import           Universum

import           Cardano.Wallet.API as API
import qualified Cardano.Wallet.API.V1.Errors as V1
import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel
import qualified Cardano.Wallet.LegacyServer as LegacyServer
import qualified Cardano.Wallet.Server as Server
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     RunMode, WalletBackendParams (..), isDebugMode,
                     walletAcidInterval, walletDbOptions)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel
import qualified Pos.Wallet.Web.Error.Types as V0

import           Control.Exception (fromException)
import           Data.Aeson
import           Formatting (build, sformat, (%))
import           Network.HTTP.Types.Status (badRequest400)
import           Network.Wai (Application, Middleware, Response, responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings,
                     setOnExceptionResponse)
import           Network.Wai.Middleware.Cors (cors, corsMethods,
                     corsRequestHeaders, simpleCorsResourcePolicy,
                     simpleMethods)
import           Ntp.Client (NtpStatus)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Wallet.Web (cleanupAcidStatePeriodically)
import           Pos.Wallet.Web.Pending.Worker (startPendingTxsResubmitter)
import qualified Pos.Wallet.Web.Server.Runner as V0
import           Pos.Wallet.Web.Sockets (getWalletWebSockets,
                     upgradeApplicationWS)
import           Servant (Context (..))
import qualified Servant

import           Pos.Context (HasNodeContext)
import           Pos.Crypto (ProtocolMagic)

import           Cardano.NodeIPC (startNodeJsIPC)
import           Pos.Configuration (walletProductionApi,
                     walletTxCreationDisabled)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (shutdownContext))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util (lensOf)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logInfo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Server.Launcher (walletDocumentationImpl,
                     walletServeImpl)
import           Pos.Wallet.Web.State (askWalletDB)
import           Pos.Wallet.Web.Tracking.Sync (processSyncRequest)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (serveWeb)
import           Pos.WorkMode (WorkMode)


-- A @Plugin@ running in the monad @m@.
type Plugin m = [Diffusion m -> m ()]

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidCleanupWorker
    :: TraceNamed IO
    -> WalletBackendParams
    -> Plugin WalletWebMode
acidCleanupWorker logTrace0 WalletBackendParams{..} = pure $ const $
    askWalletDB >>= \db -> cleanupAcidStatePeriodically logTrace db (walletAcidInterval walletDbOptions)
    where
      logTrace = appendName "acidcleanup" (natTrace liftIO logTrace0)

-- | The @Plugin@ which defines part of the conversation protocol for this node.
conversation :: HasConfigurations => WalletBackendParams -> Plugin WalletWebMode
conversation wArgs = map const (pluginsMonitoringApi wArgs)
  where
    pluginsMonitoringApi :: (WorkMode ctx m , HasNodeContext ctx)
                         => WalletBackendParams
                         -> [m ()]
    pluginsMonitoringApi WalletBackendParams {..}
        | enableMonitoringApi = [serveWeb monitoringApiPort walletTLSParams]
        | otherwise = []

walletDocumentation
    :: (HasConfigurations, HasCompileInfo)
    => TraceNamed IO
    -> WalletBackendParams
    -> Plugin WalletWebMode
walletDocumentation logTrace WalletBackendParams {..} = pure $ \_ ->
    walletDocumentationImpl
        application
        walletDocAddress
        tls
        (Just defaultSettings)
        Nothing
  where
    logTrace' = appendName "walletDocumentation" logTrace
    application :: WalletWebMode Application
    application = do
        let app = Servant.serveWithContext API.walletDocAPI (logTrace' :. EmptyContext) LegacyServer.walletDocServer
        return $ withMiddleware walletRunMode app
    tls =
        if isDebugMode walletRunMode then Nothing else walletTLSParams

-- | A @Plugin@ to start the wallet backend API.
legacyWalletBackend :: (HasConfigurations, HasCompileInfo)
                    => TraceNamed IO
                    -> ProtocolMagic
                    -> TxpConfiguration
                    -> WalletBackendParams
                    -> TVar NtpStatus
                    -> Plugin WalletWebMode
legacyWalletBackend logTrace0 pm txpConfig WalletBackendParams {..} ntpStatus = pure $ \diffusion -> do
      logInfo logTrace $ sformat ("Production mode for API: "%build)
          walletProductionApi
      logInfo logTrace $ sformat ("Transaction submission disabled: "%build)
          walletTxCreationDisabled

      ctx <- view shutdownContext
      let portCallback :: Word16 -> IO ()
          portCallback port = flip runReaderT ctx $ startNodeJsIPC logTrace' port
      walletServeImpl
          (getApplication diffusion)
          walletAddress
          -- Disable TLS if in debug mode.
          (if isDebugMode walletRunMode then Nothing else walletTLSParams)
          (Just $ setOnExceptionResponse exceptionHandler defaultSettings)
          (Just portCallback)
  where
    logTrace' = appendName "legacyWalletBackend" logTrace0
    logTrace = natTrace liftIO logTrace'
    -- Gets the Wai `Application` to run.
    getApplication :: Diffusion WalletWebMode -> WalletWebMode Application
    getApplication diffusion = do
        logInfo logTrace "Wallet Web API has STARTED!"
        wsConn <- getWalletWebSockets
        ctx <- V0.walletWebModeContext
        return
            $ withMiddleware walletRunMode
            $ upgradeApplicationWS logTrace' wsConn
            $ Servant.serveWithContext API.walletAPI (logTrace' :. EmptyContext)
            $ LegacyServer.walletServer
                (V0.convertHandler ctx)
                logTrace
                pm
                txpConfig
                diffusion
                ntpStatus
                walletRunMode

    exceptionHandler :: SomeException -> Response
    exceptionHandler se =
        case asum [handleV1Errors se, handleV0Errors se] of
             Nothing -> handleGenericError se
             Just r  -> r

    -- Handles domain-specific errors coming from the V1 API.
    handleV1Errors :: SomeException -> Maybe Response
    handleV1Errors se =
        let reify (we :: V1.WalletError) =
                responseLBS (V1.toHttpStatus we) [V1.applicationJson] .  encode $ we
        in fmap reify (fromException se)

    -- Handles domain-specific errors coming from the V0 API, but rewraps it
    -- into a jsend payload. It doesn't explicitly handle 'InternalError' or
    -- 'DecodeError', as they can come from any part of the stack or even
    -- rewrap some other exceptions (cfr 'rewrapToWalletError').
    -- Uses the 'Buildable' istance on 'WalletError' to exploit any
    -- available rendering and information-masking improvements.
    handleV0Errors :: SomeException -> Maybe Response
    handleV0Errors se =
        let maskSensitive err =
                case err of
                    V0.RequestError _  -> err
                    V0.InternalError _ -> V0.RequestError "InternalError"
                    V0.DecodeError _   -> V0.RequestError "DecodeError"
            reify (re :: V0.WalletError) = V1.UnknownError (sformat build . maskSensitive $ re)
        in fmap (responseLBS badRequest400 [V1.applicationJson] .  encode . reify) (fromException se)

    -- Handles any generic error, trying to prevent internal exceptions from leak outside.
    handleGenericError :: SomeException -> Response
    handleGenericError _ =
        responseLBS badRequest400 [V1.applicationJson] .  encode $ V1.UnknownError "Something went wrong."

-- | A 'Plugin' to start the wallet REST server
--
-- NOTE: There is no web socket support in the new wallet for now.
walletBackend :: TraceNamed IO
              -> ProtocolMagic
              -> NewWalletBackendParams
              -> (PassiveWalletLayer IO, PassiveWallet)
              -> Plugin Kernel.WalletMode
walletBackend logTrace protocolMagic (NewWalletBackendParams WalletBackendParams{..}) (passiveLayer, passiveWallet) =
    pure $ \diffusion -> do
        env <- ask
        let diffusion' = Kernel.fromDiffusion (lower env) diffusion
        WalletLayer.Kernel.bracketActiveWallet protocolMagic passiveLayer passiveWallet diffusion' $ \active _ -> do
          ctx <- view shutdownContext
          let portCallback :: Word16 -> IO ()
              portCallback port = flip runReaderT ctx $ startNodeJsIPC logTrace' port
          walletServeImpl
              (getApplication active)
              walletAddress
              -- Disable TLS if in debug modeit .
              (if isDebugMode walletRunMode then Nothing else walletTLSParams)
              Nothing
              (Just portCallback)
  where
    logTrace' = appendName "walletBackend" logTrace
    getApplication :: ActiveWalletLayer IO -> Kernel.WalletMode Application
    getApplication active = do
      logInfo (natTrace lift logTrace) "New wallet API has STARTED!"
      return $ withMiddleware walletRunMode $
        Servant.serveWithContext API.walletAPI (logTrace' :. EmptyContext) $ Server.walletServer active walletRunMode

    lower :: env -> ReaderT env IO a -> IO a
    lower env m = runReaderT m env

-- | A @Plugin@ to resubmit pending transactions.
resubmitterPlugin :: HasConfigurations
                  => TraceNamed IO
                  -> ProtocolMagic
                  -> TxpConfiguration
                  -> Plugin WalletWebMode
resubmitterPlugin logTrace pm txpConfig = [\diffusion -> askWalletDB >>= \db ->
                        startPendingTxsResubmitter (natTrace liftIO logTrace) pm txpConfig db (sendTx diffusion)]

-- | A @Plugin@ to notify frontend via websockets.
notifierPlugin :: HasConfigurations => TraceNamed IO -> Plugin WalletWebMode
notifierPlugin logTrace = [const $ V0.notifierPlugin logTrace]

-- | The @Plugin@ responsible for the restoration & syncing of a wallet.
syncWalletWorker :: HasConfigurations => TraceNamed IO -> Plugin WalletWebMode
syncWalletWorker logTrace0 = pure $ const $
    (view (lensOf @SyncQueue) >>= processSyncRequest (natTrace lift logTrace))
    where
      logTrace = appendName "syncWalletWorker" logTrace0

-- | "Attaches" the middleware to this 'Application', if any.
-- When running in debug mode, chances are we want to at least allow CORS to test the API
-- with a Swagger editor, locally.
withMiddleware :: RunMode -> Application -> Application
withMiddleware wrm app
  | isDebugMode wrm = corsMiddleware app
  | otherwise = app

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "PUT" : simpleMethods
        }
