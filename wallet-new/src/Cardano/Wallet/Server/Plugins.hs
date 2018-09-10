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

import           Network.Wai (Application, Middleware)
import           Network.Wai.Handler.Warp (defaultSettings)
import           Network.Wai.Middleware.Cors (cors, corsMethods,
                     corsRequestHeaders, simpleCorsResourcePolicy,
                     simpleMethods)

import           Cardano.NodeIPC (startNodeJsIPC)
import           Cardano.Wallet.API as API
import           Cardano.Wallet.Kernel (DatabaseMode (..), PassiveWallet)
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     RunMode, WalletBackendParams (..), getWalletDbOptions,
                     isDebugMode, walletAcidInterval)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
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

import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel
import qualified Cardano.Wallet.Server as Server
import           Cardano.Wallet.Server.Plugins.AcidState
                     (createAndArchiveCheckpoints)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel
import qualified Data.ByteString.Char8 as BS8
import qualified Servant

-- Needed for Orphan Instance 'Buildable Servant.NoContent' :|
import           Pos.Wallet.Web ()


-- A @Plugin@ running in the monad @m@.
type Plugin m = [Diffusion m -> m ()]


-- | A @Plugin@ to start the wallet REST server
apiServer
    :: ProtocolMagic
    -> NewWalletBackendParams
    -> (PassiveWalletLayer IO, PassiveWallet)
    -> Plugin Kernel.WalletMode
apiServer protocolMagic (NewWalletBackendParams WalletBackendParams{..}) (passiveLayer, passiveWallet) =
    pure $ \diffusion -> do
        env <- ask
        let diffusion' = Kernel.fromDiffusion (lower env) diffusion
        WalletLayer.Kernel.bracketActiveWallet protocolMagic passiveLayer passiveWallet diffusion' $ \active _ -> do
          ctx <- view shutdownContext
          serveImpl
            (getApplication active)
            (BS8.unpack ip)
            port
            (if isDebugMode walletRunMode then Nothing else walletTLSParams)
            Nothing
            (Just $ portCallback ctx)
  where
    (ip, port) = walletAddress

    getApplication :: ActiveWalletLayer IO -> Kernel.WalletMode Application
    getApplication active = do
      logInfo "New wallet API has STARTED!"
      return $ withMiddleware walletRunMode $
        Servant.serve API.newWalletAPI $ Server.walletServer active walletRunMode

    lower :: env -> ReaderT env IO a -> IO a
    lower env m = runReaderT m env

    portCallback :: ShutdownContext -> Word16 -> IO ()
    portCallback ctx =
        usingLoggerName "NodeIPC" . flip runReaderT ctx . startNodeJsIPC

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


-- | A @Plugin@ to serve the wallet documentation
docServer
    :: (HasConfigurations, HasCompileInfo)
    => NewWalletBackendParams
    -> Plugin Kernel.WalletMode
docServer (NewWalletBackendParams WalletBackendParams{..}) = pure $ \_ ->
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
        return $ Servant.serve API.newWalletDocAPI Server.walletDocServer

-- | A @Plugin@ to serve the node monitoring API.
monitoringServer :: HasConfigurations
                 => NewWalletBackendParams
                 -> Plugin Kernel.WalletMode
monitoringServer (NewWalletBackendParams WalletBackendParams{..}) =
    case enableMonitoringApi of
         True  -> pure $ \_ -> do
             serveImpl Pos.Web.Server.application
                       "127.0.0.1"
                       monitoringApiPort
                       walletTLSParams
                       Nothing
                       Nothing
         False -> []

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidStateSnapshots :: AcidState db
                   -> NewWalletBackendParams
                   -> DatabaseMode
                   -> Plugin Kernel.WalletMode
acidStateSnapshots dbRef params dbMode = pure $ \_diffusion -> do
    let opts = getWalletDbOptions params
    modifyLoggerName (const "acid-state-checkpoint-plugin") $
        createAndArchiveCheckpoints
            dbRef
            (walletAcidInterval opts)
            dbMode

-- | A @Plugin@ to store updates proposal received from the blockchain
updateWatcher :: Plugin Kernel.WalletMode
updateWatcher = pure $ \_diffusion -> do
    modifyLoggerName (const "update-watcher-plugin") $ do
        w <- Kernel.getWallet
        forever $ liftIO $ do
            newUpdate <- WalletLayer.waitForUpdate w
            logInfo "A new update was found!"
            WalletLayer.addUpdate w . cpsSoftwareVersion $ newUpdate
