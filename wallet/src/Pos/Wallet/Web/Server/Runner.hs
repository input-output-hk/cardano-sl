{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RealMode`-related part of full-node implementation of
-- Daedalus API.

module Pos.Wallet.Web.Server.Runner
       ( walletServeWebFull
       , runWRealMode
       ) where

import           Universum                      hiding (over)

import qualified Control.Monad.Catch            as Catch
import           Control.Monad.Except           (MonadError (throwError))
import qualified Control.Monad.Reader           as Mtl
import           Ether.Internal                 (HasLens (..))
import           Mockable                       (Production, runProduction)
import           Network.Wai                    (Application)
import           Servant.Server                 (Handler)
import           Servant.Utils.Enter            ((:~>) (..))
import           System.Wlog                    (logInfo)

import           Pos.Communication              (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol     (SendActions)
import           Pos.Launcher.Configuration     (HasConfigurations)
import           Pos.Launcher.Resource          (NodeResources)
import           Pos.Launcher.Runner            (runRealBasedMode)
import           Pos.Util.TimeWarp              (NetworkAddress)
import           Pos.Wallet.SscType             (WalletSscType)
import           Pos.Wallet.Web.Methods         (addInitialRichAccount)
import           Pos.Wallet.Web.Mode            (AddrCIdHashes (..), WalletWebMode,
                                                 WalletWebModeContext (..),
                                                 WalletWebModeContextTag)
import           Pos.Wallet.Web.Server.Launcher (walletApplication, walletServeImpl,
                                                 walletServer)
import           Pos.Wallet.Web.Sockets         (ConnectionsVar)
import           Pos.Wallet.Web.State           (WalletState)
import           Pos.Web                        (TlsParams)

-- | 'WalletWebMode' runner.
runWRealMode
    :: HasConfigurations
    => WalletState
    -> ConnectionsVar
    -> NodeResources WalletSscType WalletWebMode
    -> (ActionSpec WalletWebMode a, OutSpecs)
    -> Production a
runWRealMode db conn res acts = do
    ref <- newIORef mempty
    runRealBasedMode
        (Mtl.withReaderT (WalletWebModeContext db conn $ AddrCIdHashes ref))
        (Mtl.withReaderT (\(WalletWebModeContext _ _ _ rmc) -> rmc))
        res
        acts

walletServeWebFull
    :: HasConfigurations
    => SendActions WalletWebMode
    -> Bool              -- whether to include genesis keys
    -> NetworkAddress    -- ^ IP and Port to listen
    -> Maybe TlsParams
    -> WalletWebMode ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletWebMode Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when debug $ addInitialRichAccount 0
        walletApplication $ walletServer @WalletWebMode sendActions nat

nat :: WalletWebMode (WalletWebMode :~> Handler)
nat = do
    wwmc <- view (lensOf @WalletWebModeContextTag)
    pure $ NT (convertHandler wwmc)

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
