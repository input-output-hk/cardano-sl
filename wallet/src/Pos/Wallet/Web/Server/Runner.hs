{-# LANGUAGE AllowAmbiguousTypes #-}
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
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (shutdownContext))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, logInfo)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import qualified Pos.Wallet.Web.Methods as M
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
    :: forall a .
       ( HasConfigurations
       , HasCompileInfo
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> WalletDB
    -> ConnectionsVar
    -> SyncQueue
    -> NodeResources WalletMempoolExt
    -> (Diffusion WalletWebMode -> WalletWebMode a)
    -> IO a
runWRealMode logTrace pm txpConfig db conn syncRequests res action =
    runRealMode logTrace pm txpConfig res $ \diffusion ->
        walletWebModeToRealMode db conn syncRequests $
            action (hoistDiffusion realModeToWalletWebMode (walletWebModeToRealMode db conn syncRequests) diffusion)

walletServeWebFull
    :: forall ctx m .
       ( HasConfigurations
       , HasCompileInfo
       , M.MonadWalletLogic ctx m
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> Diffusion WalletWebMode
    -> TVar NtpStatus
    -> Bool                    -- ^ whether to include genesis keys
    -> NetworkAddress          -- ^ IP and Port to listen
    -> Maybe TlsParams
    -> WalletWebMode ()
walletServeWebFull logTrace pm txpConfig diffusion ntpStatus debug address mTlsParams = do
    ctx <- view shutdownContext
    let portCallback :: Word16 -> IO ()
        portCallback port = flip runReaderT ctx $ startNodeJsIPC logTrace port
    walletServeImpl action address mTlsParams Nothing (Just portCallback)
  where
    action :: WalletWebMode Application
    action = do
        logInfo (natTrace liftIO logTrace) "Wallet Web API has STARTED!"
        when debug $ M.addInitialRichAccount (natTrace liftIO logTrace) 0

        wwmc <- walletWebModeContext
        walletApplication logTrace $
            walletServer @WalletWebModeContext @WalletWebMode
                (natTrace liftIO logTrace) pm txpConfig diffusion ntpStatus (convertHandler wwmc)

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

notifierPlugin
    :: (HasConfigurations)
    => TraceNamed IO
    -> WalletWebMode ()
notifierPlugin logTrace = do
    wwmc <- walletWebModeContext
    launchNotifier (natTrace liftIO logTrace) (convertHandler wwmc)
