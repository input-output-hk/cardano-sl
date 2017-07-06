{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RealMode`-related part of full-node implementation of
-- Daedalus API.

module Pos.Wallet.Web.Server.Full
       ( walletServeWebFull
       , runWRealMode
       ) where

import           Universum                     hiding (over)

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import qualified Control.Monad.Reader          as Mtl
import           Ether.Internal                (HasLens (..))
import           Mockable                      (Production, runProduction)
import           Network.Wai                   (Application)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (logInfo)

import           Pos.Communication             (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol    (SendActions)
import           Pos.Launcher.Resource         (NodeResources)
import           Pos.Launcher.Runner           (runRealBasedMode)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.Web.Mode           (WalletWebMode, WalletWebModeContext (..),
                                                WalletWebModeContextTag)
import           Pos.Wallet.Web.Server.Methods (addInitialRichAccount, walletApplication,
                                                walletServeImpl, walletServer)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar)
import           Pos.Wallet.Web.State          (WalletState)

-- | WalletWebMode runner.
runWRealMode
    :: WalletState
    -> ConnectionsVar
    -> NodeResources WalletSscType WalletWebMode
    -> (ActionSpec WalletWebMode a, OutSpecs)
    -> Production a
runWRealMode db conn =
    runRealBasedMode
        (Mtl.withReaderT (WalletWebModeContext db conn))
        (Mtl.withReaderT (\(WalletWebModeContext _ _ rmc) -> rmc))

walletServeWebFull
    :: SendActions WalletWebMode
    -> Bool      -- whether to include genesis keys
    -> Word16
    -> WalletWebMode ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletWebMode Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when debug $ addInitialRichAccount 0
        walletApplication $ walletServer @WalletWebModeContext sendActions nat

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
