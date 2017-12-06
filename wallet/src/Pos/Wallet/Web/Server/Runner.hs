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

import           Universum                         hiding (over)

import qualified Control.Concurrent.STM            as STM
import qualified Control.Monad.Catch               as Catch
import           Control.Monad.Except              (MonadError (throwError))
import qualified Control.Monad.Reader              as Mtl
import           Ether.Internal                    (HasLens (..))
import           Mockable                          (Production, runProduction)
import           Network.Wai                       (Application)
import           Servant.Server                    (Handler)
import           Servant.Utils.Enter               ((:~>) (..))
import           System.Wlog                       (logInfo)

import           Pos.Communication                 (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol        (SendActions)
import qualified Pos.DB.GState.Common              as DB
import           Pos.Launcher.Configuration        (HasConfigurations)
import           Pos.Launcher.Resource             (NodeResources)
import           Pos.Launcher.Runner               (runRealBasedMode)
import           Pos.Util.CompileInfo              (HasCompileInfo)
import           Pos.Util.TimeWarp                 (NetworkAddress)
import           Pos.Wallet.WalletMode             (WalletMempoolExt)
import           Pos.Wallet.Web.Methods            (addInitialRichAccount)
import           Pos.Wallet.Web.Mode               (WalletWebMode,
                                                    WalletWebModeContext (..),
                                                    WalletWebModeContextTag)
import           Pos.Wallet.Web.Server.Launcher    (walletApplication, walletServeImpl,
                                                    walletServer)
import           Pos.Wallet.Web.Sockets            (ConnectionsVar)
import           Pos.Wallet.Web.State              (WalletState)
import           Pos.Wallet.Web.State.Memory.Types (ExtStorageModifier (..))
import           Pos.Web                           (TlsParams)

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
    blocksModifierVar <- atomically $ STM.newTVar mempty
    memStorageModifierVar <- atomically $ STM.newEmptyTMVar
    runRealBasedMode
        (Mtl.withReaderT (WalletWebModeContext db conn saVar memStorageModifierVar blocksModifierVar))
        (Mtl.withReaderT (\(WalletWebModeContext _ _ _ _ _ rmc) -> rmc))
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
        logInfo "DAEDALUS has STARTED!"
        initialization
        when debug $ addInitialRichAccount 0
        walletApplication $
            walletServer @WalletWebModeContext @WalletWebMode nat

    initialization :: WalletWebMode ()
    initialization = do
        saVar <- asks wwmcSendActions
        atomically $ STM.putTMVar saVar sendActions
        tip <- DB.getTip
        memStorageModifierVar <- asks wwmcMemStorageModifier
        atomically $ STM.putTMVar memStorageModifierVar (ExtStorageModifier tip mempty)

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
