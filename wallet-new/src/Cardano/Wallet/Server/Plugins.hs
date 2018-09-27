{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}

module Cardano.Wallet.Server.Plugins
    ( Plugin
    , apiServer
    , docServer
    , monitoringServer
    , acidStateSnapshots
    , updateWatcher
    ) where

import           Universum

import           Data.Acid (AcidState)
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import           Data.Typeable (typeOf)
import qualified Servant

import           Network.HTTP.Types.Status (badRequest400)
import           Network.Wai (Application, Middleware, Response, responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings,
                     setOnExceptionResponse)

import           Cardano.NodeIPC (startNodeJsIPC)
import           Cardano.Wallet.API as API
import           Cardano.Wallet.API.V1.Headers (applicationJson)
import           Cardano.Wallet.API.V1.ReifyWalletError
                     (translateWalletLayerErrors)
import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel (DatabaseMode (..), PassiveWallet)
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel
import qualified Cardano.Wallet.Server as Server
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     WalletBackendParams (..), getWalletDbOptions, isDebugMode,
                     walletAcidInterval)
import           Cardano.Wallet.Server.Middlewares (withMiddlewares)
import           Cardano.Wallet.Server.Plugins.AcidState
                     (createAndArchiveCheckpoints)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel

import           Pos.Chain.Update (cpsSoftwareVersion)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Infra.Shutdown (HasShutdownContext (shutdownContext),
                     ShutdownContext)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Wlog (logInfo, modifyLoggerName, usingLoggerName)
import           Pos.Web (serveDocImpl, serveImpl)
import qualified Pos.Web.Server

-- Needed for Orphan Instance 'Buildable Servant.NoContent' :|
import           Pos.Wallet.Web ()


-- A @Plugin@ running in the monad @m@.
type Plugin m = Diffusion m -> m ()


-- | A @Plugin@ to start the wallet REST server
apiServer
    :: ProtocolMagic
    -> NewWalletBackendParams
    -> (PassiveWalletLayer IO, PassiveWallet)
    -> [Middleware]
    -> Plugin Kernel.WalletMode
apiServer protocolMagic (NewWalletBackendParams WalletBackendParams{..}) (passiveLayer, passiveWallet) middlewares diffusion = do
        env <- ask
        let diffusion' = Kernel.fromDiffusion (lower env) diffusion
        WalletLayer.Kernel.bracketActiveWallet protocolMagic passiveLayer passiveWallet diffusion' $ \active _ -> do
          ctx <- view shutdownContext
          serveImpl
            (getApplication active)
            (BS8.unpack ip)
            port
            (if isDebugMode walletRunMode then Nothing else walletTLSParams)
            (Just $ setOnExceptionResponse exceptionHandler defaultSettings)
            (Just $ portCallback ctx)
  where
    (ip, port) = walletAddress

    exceptionHandler :: SomeException -> Response
    exceptionHandler se = case translateWalletLayerErrors se of
            Just we -> handleLayerError we
            Nothing -> handleGenericError se

    -- Handle domain-specific errors coming from the Wallet Layer
    handleLayerError :: V1.WalletError -> Response
    handleLayerError we =
            responseLBS (V1.toHttpErrorStatus we) [applicationJson] . encode $ we

    -- Handle general exceptions
    handleGenericError :: SomeException -> Response
    handleGenericError se =
        responseLBS badRequest400 [applicationJson] $ encode defWalletError
        where
            -- NOTE: to ensure that we don't leak any sensitive information,
            --       we only reveal the exception type here.
            defWalletError = V1.UnknownError $ T.pack . show $ typeOf se

    getApplication :: ActiveWalletLayer IO -> Kernel.WalletMode Application
    getApplication active = do
        logInfo "New wallet API has STARTED!"
        return
            $ withMiddlewares middlewares
            $ Servant.serve API.walletAPI
            $ Server.walletServer active walletRunMode

    lower :: env -> ReaderT env IO a -> IO a
    lower env m = runReaderT m env

    portCallback :: ShutdownContext -> Word16 -> IO ()
    portCallback ctx =
        usingLoggerName "NodeIPC" . flip runReaderT ctx . startNodeJsIPC

-- | A @Plugin@ to serve the wallet documentation
docServer
    :: (HasConfigurations, HasCompileInfo)
    => NewWalletBackendParams
    -> Plugin Kernel.WalletMode
docServer (NewWalletBackendParams WalletBackendParams{..}) = const $
    serveDocImpl
        application
        (BS8.unpack ip)
        port
        (if isDebugMode walletRunMode then Nothing else walletTLSParams)
        (Just defaultSettings)
        Nothing
  where
    (ip, port) = walletDocAddress

    application :: Kernel.WalletMode Application
    application =
        return $ Servant.serve API.walletDoc Server.walletDocServer

-- | A @Plugin@ to serve the node monitoring API.
monitoringServer :: HasConfigurations
                 => NewWalletBackendParams
                 -> [ (Text, Plugin Kernel.WalletMode) ]
monitoringServer (NewWalletBackendParams WalletBackendParams{..}) =
    case enableMonitoringApi of
         True  -> [ ("monitoring worker", const worker) ]
         False -> []
  where
    worker = serveImpl Pos.Web.Server.application
                       "127.0.0.1"
                       monitoringApiPort
                       walletTLSParams
                       Nothing
                       Nothing

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidStateSnapshots :: AcidState db
                   -> NewWalletBackendParams
                   -> DatabaseMode
                   -> Plugin Kernel.WalletMode
acidStateSnapshots dbRef params dbMode = const worker
  where
    worker = do
      let opts = getWalletDbOptions params
      modifyLoggerName (const "acid-state-checkpoint-plugin") $
          createAndArchiveCheckpoints
              dbRef
              (walletAcidInterval opts)
              dbMode

-- | A @Plugin@ to store updates proposal received from the blockchain
updateWatcher :: Plugin Kernel.WalletMode
updateWatcher = const $ do
    modifyLoggerName (const "update-watcher-plugin") $ do
        w <- Kernel.getWallet
        forever $ liftIO $ do
            newUpdate <- WalletLayer.waitForUpdate w
            logInfo "A new update was found!"
            WalletLayer.addUpdate w . cpsSoftwareVersion $ newUpdate
