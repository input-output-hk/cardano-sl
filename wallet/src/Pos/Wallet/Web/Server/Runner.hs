{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RealMode`-related part of full-node implementation of
-- Daedalus API.

module Pos.Wallet.Web.Server.Runner
       ( runWRealMode
       , walletWebModeContext
       , convertHandler
       , notifierPlugin
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Servant.Server (Handler)

import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Launcher (HasConfigurations, NodeResources (..))
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..),
                     WalletWebModeContextTag, realModeToWalletWebMode,
                     walletWebModeToRealMode)
import           Pos.Wallet.Web.Sockets (ConnectionsVar, launchNotifier)
import           Pos.Wallet.Web.State (WalletDB)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import qualified Control.Exception.Safe as E
import qualified Control.Monad.Reader as Mtl


-- | 'WalletWebMode' runner.
runWRealMode
    :: forall a .
       ( HasConfigurations
       , HasCompileInfo
       )
    => ProtocolMagic
    -> WalletDB
    -> ConnectionsVar
    -> SyncQueue
    -> NodeResources WalletMempoolExt
    -> (Diffusion WalletWebMode -> WalletWebMode a)
    -> IO a
runWRealMode pm db conn syncRequests res action =
    runRealMode pm res $ \diffusion ->
        walletWebModeToRealMode db conn syncRequests $
            action (hoistDiffusion realModeToWalletWebMode (walletWebModeToRealMode db conn syncRequests) diffusion)

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

notifierPlugin :: (HasConfigurations) => WalletWebMode ()
notifierPlugin = do
    wwmc <- walletWebModeContext
    launchNotifier (convertHandler wwmc)
