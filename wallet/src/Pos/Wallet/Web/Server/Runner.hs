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

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as E
import           Control.Monad.Except (MonadError (throwError))
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal (HasLens (..))
import           Mockable (Production, runProduction)
import           Network.Wai (Application)
import           Servant.Server (Handler)
import           System.Wlog (logInfo)

import           Pos.Communication (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol (SendActions)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Resource (NodeResources)
import           Pos.Launcher.Runner (runRealBasedMode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.TimeWarp (NetworkAddress)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (AddrCIdHashes (..), addInitialRichAccount)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..),
                                      WalletWebModeContextTag)
import           Pos.Wallet.Web.Server.Launcher (walletApplication, walletServeImpl, walletServer)
import           Pos.Wallet.Web.Sockets (ConnectionsVar, launchNotifier)
import           Pos.Wallet.Web.State (WalletState)
import           Pos.Web (TlsParams)

-- | 'WalletWebMode' runner.
runWRealMode
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => WalletState
    -> ConnectionsVar
    -> NodeResources WalletMempoolExt WalletWebMode
    -> (ActionSpec WalletWebMode a, OutSpecs)
    -> Production a
runWRealMode db conn res spec = do
    saVar <- atomically STM.newEmptyTMVar
    ref <- newIORef mempty
    runRealBasedMode
        (Mtl.withReaderT (WalletWebModeContext db conn (AddrCIdHashes ref) saVar))
        (Mtl.withReaderT (\(WalletWebModeContext _ _ _ _ rmc) -> rmc))
        res
        spec

walletServeWebFull
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SendActions WalletWebMode
    -> Bool              -- whether to include genesis keys
    -> NetworkAddress    -- ^ IP and Port to listen
    -> Maybe TlsParams
    -> WalletWebMode ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletWebMode Application
    action = do
        logInfo "Wallet Web API has STARTED!"
        saVar <- asks wwmcSendActions
        atomically $ STM.putTMVar saVar sendActions
        when debug $ addInitialRichAccount 0

        wwmc <- walletWebModeContext
        walletApplication $
            walletServer @WalletWebModeContext @WalletWebMode (convertHandler wwmc)

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
