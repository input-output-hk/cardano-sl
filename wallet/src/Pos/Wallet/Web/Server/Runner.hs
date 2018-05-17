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
import           Mockable (Production (..), runProduction)
import           Network.Wai (Application)
import           Ntp.Client (NtpStatus)
import           Servant.Server (Handler)
import           System.Wlog (logInfo, usingLoggerName)

import           Pos.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.TimeWarp (NetworkAddress)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (addInitialRichAccount)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..),
                                      WalletWebModeContextTag, realModeToWalletWebMode,
                                      walletWebModeToRealMode)
import           Pos.Wallet.Web.Server.Launcher (walletApplication, walletServeImpl, walletServer)
import           Pos.Wallet.Web.Sockets (ConnectionsVar, launchNotifier)
import           Pos.Wallet.Web.State (WalletDB)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (TlsParams)
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
    -> (Diffusion WalletWebMode -> WalletWebMode a)
    -> Production a
runWRealMode db conn syncRequests res action = Production $
    runRealMode res $ \diffusion ->
        walletWebModeToRealMode db conn syncRequests $
            action (hoistDiffusion realModeToWalletWebMode diffusion)

walletServeWebFull
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Diffusion WalletWebMode
    -> TVar NtpStatus
    -> Bool                    -- ^ whether to include genesis keys
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

notifierPlugin :: (HasConfigurations) => WalletWebMode ()
notifierPlugin = do
    wwmc <- walletWebModeContext
    launchNotifier (convertHandler wwmc)
