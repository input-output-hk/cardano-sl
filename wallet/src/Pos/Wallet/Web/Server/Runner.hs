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
       ) where

import           Universum

import qualified Control.Monad.Catch as Catch
import           Control.Monad.Except (MonadError (throwError))
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal (HasLens (..))
import           Mockable (Production, runProduction)
import           Network.Wai (Application)
import           Servant.Server (Handler)
import           System.Wlog (logInfo)

import           Pos.Communication (ActionSpec (..), OutSpecs)
import           Pos.Context (NodeContext (..))
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Resource (NodeResources (..), hoistNodeResources)
import           Pos.Launcher.Runner (elimRealMode, runServer)
import           Pos.Reporting.Ekg (EkgNodeMetrics (..))
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.TimeWarp (NetworkAddress)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (addInitialRichAccount)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..),
                                      WalletWebModeContextTag, walletWebModeToRealMode)
import           Pos.Wallet.Web.Server.Launcher (walletApplication, walletServeImpl, walletServer)
import           Pos.Wallet.Web.Sockets (ConnectionsVar)
import           Pos.Wallet.Web.State (WalletState)
import           Pos.Web (TlsParams)
import           Pos.WorkMode (RealMode)

-- | 'WalletWebMode' runner.
runWRealMode
    :: forall a .
       ( HasConfigurations
       , HasCompileInfo
       )
    => WalletState
    -> ConnectionsVar
    -> NodeResources WalletMempoolExt WalletWebMode
    -> (ActionSpec WalletWebMode a, OutSpecs)
    -> Production a
runWRealMode db conn res (action, outSpecs) =
    elimRealMode hoistedNr serverRealMode
  where
    NodeContext {..} = nrContext res
    ekgNodeMetrics = EkgNodeMetrics
        (nrEkgStore res)
        (runProduction . elimRealMode hoistedNr . walletWebModeToRealMode db conn)
    hoistedNr = hoistNodeResources nat res
    serverWalletWebMode :: WalletWebMode a
    serverWalletWebMode = runServer ncNodeParams ekgNodeMetrics outSpecs action
    serverRealMode :: RealMode WalletMempoolExt a
    serverRealMode = walletWebModeToRealMode db conn serverWalletWebMode
    nat :: forall t . WalletWebMode t -> RealMode WalletMempoolExt t
    nat = Mtl.withReaderT (\rmc -> (WalletWebModeContext db conn rmc))

walletServeWebFull
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Diffusion WalletWebMode
    -> Bool                    -- whether to include genesis keys
    -> NetworkAddress          -- ^ IP and Port to listen
    -> Maybe TlsParams
    -> WalletWebMode ()
walletServeWebFull diffusion debug = walletServeImpl action
  where
    action :: WalletWebMode Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when debug $ addInitialRichAccount 0

        wwmc <- walletWebModeContext
        walletApplication $
            walletServer @WalletWebModeContext @WalletWebMode diffusion (convertHandler wwmc)

walletWebModeContext :: WalletWebMode WalletWebModeContext
walletWebModeContext = view (lensOf @WalletWebModeContextTag)

convertHandler
    :: WalletWebModeContext
    -> WalletWebMode a
    -> Handler a
convertHandler wwmc handler =
    liftIO (walletRunner handler) `Catch.catches` excHandlers
  where

    walletRunner :: forall a . WalletWebMode a -> IO a
    walletRunner act = runProduction $
        Mtl.runReaderT act wwmc

    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
