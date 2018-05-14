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
import           Mockable (Production, runProduction)
import           Network.Wai (Application)
import           Ntp.Client (NtpStatus)
import           Servant.Server (Handler)
import           System.Wlog (logInfo, usingLoggerName)

import           Pos.Communication (ActionSpec (..), OutSpecs)
import           Pos.Context (NodeContext (..))
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Launcher.Runner (elimRealMode, runServer)
import           Pos.Reporting.Ekg (EkgNodeMetrics (..))
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.TimeWarp (NetworkAddress)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (addInitialRichAccount)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..),
                                      WalletWebModeContextTag, walletWebModeToRealMode)
import           Pos.Wallet.Web.Server.Launcher (walletApplication, walletServeImpl, walletServer)
import           Pos.Wallet.Web.Sockets (ConnectionsVar, launchNotifier)
import           Pos.Wallet.Web.State (WalletDB)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (TlsParams)
import           Pos.WorkMode (RealMode)
import           Cardano.NodeIPC (startNodeJsIPC)
import           Pos.Shutdown.Class        (HasShutdownContext (shutdownContext))

-- | 'WalletWebMode' runner.
runWRealMode
    :: forall a .
       ( HasConfigurations
       , HasCompileInfo
       )
    => WalletDB
    -> ConnectionsVar
    -> SyncQueue
    -> NodeResources WalletMempoolExt
    -> (ActionSpec WalletWebMode a, OutSpecs)
    -> Production a
runWRealMode db conn syncRequests res (action, outSpecs) =
    elimRealMode res serverRealMode
  where
    NodeContext {..} = nrContext res
    ekgNodeMetrics = EkgNodeMetrics (nrEkgStore res)
    serverWalletWebMode :: WalletWebMode a
    serverWalletWebMode = runServer
        (runProduction . elimRealMode res . walletWebModeToRealMode db conn syncRequests)
        ncNodeParams
        ekgNodeMetrics
        outSpecs
        action
    serverRealMode :: RealMode WalletMempoolExt a
    serverRealMode = walletWebModeToRealMode db conn syncRequests serverWalletWebMode

walletServeWebFull
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Diffusion WalletWebMode
    -> TVar NtpStatus
    -> Bool                    -- whether to include genesis keys
    -> NetworkAddress          -- ^ IP and Port to listen
    -> Maybe TlsParams
    -> WalletWebMode ()
walletServeWebFull diffusion ntpStatus debug address mTlsParams = do
    ctx <- view shutdownContext
    let
      portCallback :: Word16 -> IO ()
      portCallback port = usingLoggerName "NodeIPC" $ flip runReaderT ctx $ startNodeJsIPC port
    walletServeImpl action address mTlsParams Nothing (Just portCallback)
  where
    action :: WalletWebMode Application
    action = do
        logInfo "Wallet Web API has STARTED!"
        when debug $ addInitialRichAccount 0

        wwmc <- walletWebModeContext
        walletApplication $
            walletServer @WalletWebModeContext @WalletWebMode diffusion ntpStatus (convertHandler wwmc)

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
    walletRunner act = runProduction $
        Mtl.runReaderT act wwmc

    excHandlers = [E.Handler catchServant]
    catchServant = throwError

notifierPlugin :: (HasConfigurations, HasCompileInfo) => WalletWebMode ()
notifierPlugin = do
    wwmc <- walletWebModeContext
    launchNotifier (convertHandler wwmc)
