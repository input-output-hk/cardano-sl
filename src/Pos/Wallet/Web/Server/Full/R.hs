{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RealMode`-related part of full-node implementation of
-- Daedalus API. FYI: R stays for Real.

module Pos.Wallet.Web.Server.Full.R
       ( walletServeWebFull
       , runWRealMode
       ) where

import           Universum                         hiding (over)

import           Control.Lens                      (over)
import qualified Control.Monad.Reader              as Mtl
import           Mockable                          (Production)
import           Network.Wai                       (Application)
import           System.Wlog                       (logInfo)

import           Pos.Communication                 (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol        (SendActions)
import           Pos.ExecMode                      (_ExecMode)
import           Pos.Launcher.Resource             (NodeResources)
import           Pos.Launcher.Runner               (runRealBasedMode)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Mode               (WalletWebMode,
                                                    WalletWebModeContext (..))
import           Pos.Wallet.Web.Server.Full.Common (nat)
import           Pos.Wallet.Web.Server.Methods     (addInitialRichAccount,
                                                    walletApplication, walletServeImpl,
                                                    walletServer)
import           Pos.Wallet.Web.Server.Sockets     (ConnectionsVar)
import           Pos.Wallet.Web.State              (WalletState)

-- | WalletWebMode runner.
runWRealMode
    :: WalletState
    -> ConnectionsVar
    -> NodeResources WalletSscType WalletWebMode
    -> (ActionSpec WalletWebMode a, OutSpecs)
    -> Production a
runWRealMode db conn =
    runRealBasedMode
        (over _ExecMode (Mtl.withReaderT (WalletWebModeContext db conn)))
        (over _ExecMode (Mtl.withReaderT wmcRealModeContext))

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
        walletApplication $ walletServer sendActions nat
