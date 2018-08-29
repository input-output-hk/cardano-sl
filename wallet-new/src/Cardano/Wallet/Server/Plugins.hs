{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}

module Cardano.Wallet.Server.Plugins
    ( Plugin
    , apiServer
    , docServer
    , acidStateSnapshots
    , updateNotifier
    ) where

import           Universum

import           Network.Wai (Application, Middleware)
import           Network.Wai.Handler.Warp (defaultSettings)
import           Network.Wai.Middleware.Cors (cors, corsMethods,
                     corsRequestHeaders, simpleCorsResourcePolicy,
                     simpleMethods)

import           Cardano.NodeIPC (startNodeJsIPC)
import           Cardano.Wallet.API as API
import           Cardano.Wallet.Kernel (PassiveWallet)
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     RunMode, WalletBackendParams (..), isDebugMode)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Infra.Shutdown (HasShutdownContext (shutdownContext),
                     ShutdownContext)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Wlog (logError, logInfo, usingLoggerName)
import           Pos.Web (serveDocImpl, serveImpl)

import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel
import qualified Cardano.Wallet.Server as Server
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
    (ip, port) = walletDocAddress

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
    (ip, port) = walletAddress

    application :: Kernel.WalletMode Application
    application =
        return $ Servant.serve API.newWalletDocAPI Server.walletDocServer


-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidStateSnapshots :: Plugin Kernel.WalletMode
acidStateSnapshots = [
    \_diffusion -> logError "Not Implemented: acidStateSnapshots [CBR-305]"
    ]

-- | A @Plugin@ to notify frontend via websockets.
updateNotifier :: Plugin Kernel.WalletMode
updateNotifier = [
    \_diffusion -> logError "Not Implemented: updateNotifier [CBR-374]"
    ]
