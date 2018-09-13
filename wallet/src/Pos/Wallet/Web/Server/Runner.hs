{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RealMode`-related part of full-node implementation of
-- Daedalus API.

module Pos.Wallet.Web.Server.Runner
       ( walletServeWebFull
       , runWRealMode
       , walletWebModeContext
       , convertHandler
       , notifierPlugin
       ) where

import           Universum

import qualified Control.Exception.Safe as E
import           Control.Monad.Except (MonadError (throwError))
import qualified Control.Monad.Reader as Mtl
import           Network.Wai (Application)
import           Servant.Server (Handler)

import           Ntp.Client (NtpStatus)

import           Cardano.NodeIPC (startNodeJsIPC)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core as Core (Config (..), configGeneratedSecretsThrow)
import           Pos.Core.Genesis (gsPoorSecrets)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (shutdownContext))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Util (HasLens (..))
import           Pos.Util.Wlog (logInfo, usingLoggerName)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (addInitialRichAccount)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..),
                     WalletWebModeContextTag, realModeToWalletWebMode,
                     walletWebModeToRealMode)
import           Pos.Wallet.Web.Server.Launcher (walletApplication,
                     walletServeImpl, walletServer)
import           Pos.Wallet.Web.Sockets (ConnectionsVar, launchNotifier)
import           Pos.Wallet.Web.State (WalletDB)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (TlsParams)

-- | 'WalletWebMode' runner.
runWRealMode
    :: forall a
     . (HasConfigurations, HasCompileInfo)
    => Core.Config
    -> TxpConfiguration
    -> WalletDB
    -> ConnectionsVar
    -> SyncQueue
    -> NodeResources WalletMempoolExt
    -> (Diffusion WalletWebMode -> WalletWebMode a)
    -> IO a
runWRealMode coreConfig txpConfig db conn syncRequests res action =
    runRealMode coreConfig txpConfig res $ \diffusion ->
        walletWebModeToRealMode db conn syncRequests $
            action (hoistDiffusion realModeToWalletWebMode (walletWebModeToRealMode db conn syncRequests) diffusion)

walletServeWebFull
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Core.Config
    -> TxpConfiguration
    -> Diffusion WalletWebMode
    -> TVar NtpStatus
    -> Bool                    -- ^ whether to include genesis keys
    -> NetworkAddress          -- ^ IP and Port to listen
    -> Maybe TlsParams
    -> WalletWebMode ()
walletServeWebFull coreConfig txpConfig diffusion ntpStatus debug address mTlsParams = do
    ctx <- view shutdownContext
    let
      portCallback :: Word16 -> IO ()
      portCallback port = usingLoggerName "NodeIPC" $ flip runReaderT ctx $ startNodeJsIPC port
    walletServeImpl action address mTlsParams Nothing (Just portCallback)
  where
    action :: WalletWebMode Application
    action = do
        logInfo "Wallet Web API has STARTED!"
        when debug $ do
          generatedSecrets <- gsPoorSecrets
              <$> configGeneratedSecretsThrow coreConfig
          addInitialRichAccount 0 coreConfig generatedSecrets

        wwmc <- walletWebModeContext
        walletApplication $ walletServer @WalletWebModeContext @WalletWebMode
            coreConfig
            txpConfig
            diffusion
            ntpStatus
            (convertHandler wwmc)

walletWebModeContext :: WalletWebMode WalletWebModeContext
walletWebModeContext = view (lensOf @WalletWebModeContextTag)

convertHandler
    :: WalletWebModeContext
    -> WalletWebMode a
    -> Handler a
convertHandler wwmc handler =
    liftIO (walletRunner handler) `E.catches` excHandlers
  where

    walletRunner :: forall a . WalletWebMode a -> IO a
    walletRunner act =
        Mtl.runReaderT act wwmc

    excHandlers = [E.Handler catchServant]
    catchServant = throwError

notifierPlugin :: WalletWebMode ()
notifierPlugin = do
    wwmc <- walletWebModeContext
    launchNotifier (convertHandler wwmc)
