{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RealMode`-related part of full-node implementation of
-- Daedalus API. FYI: R stays for Real.

module Pos.Wallet.Web.Server.Full.R
       ( walletServeWebFull
       , runWRealMode
       , WalletRealWebMode
       ) where

import           Universum

import           Mockable                          (Production)
import           Network.Transport.Abstract        (Transport)
import           Network.Wai                       (Application)
import           System.Wlog                       (logInfo)

import           Pos.Communication                 (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol        (SendActions)
import           Pos.Discovery                     (DiscoveryContextSum)
import           Pos.Launcher.Param                (NodeParams (..))
import           Pos.Launcher.Runner               (runRealBasedMode)
import           Pos.Ssc.Class                     (SscConstraint, SscParams)
import           Pos.Wallet.Redirect               (liftWalletRedirects,
                                                    runWalletRedirects)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Server.Full.Common (nat)
import           Pos.Wallet.Web.Server.Methods     (WalletWebHandler,
                                                    addInitialRichAccount,
                                                    walletApplication, walletServeImpl,
                                                    walletServer)
import           Pos.Wallet.Web.Server.Sockets     (ConnectionsVar, runWalletWS)
import           Pos.Wallet.Web.State              (WalletState, runWalletWebDB)
import           Pos.WorkMode                      (RealMode)

type WalletRealWebMode = WalletWebHandler (RealMode WalletSscType)

-- | WalletRealWebMode runner.
runWRealMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> DiscoveryContextSum
    -> Transport WalletRealWebMode
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletRealWebMode a, OutSpecs)
    -> Production a
runWRealMode db conn =
    runRealBasedMode
        unwrapWPMode
        -- doesn't work
        -- (powerLift @(RealMode WalletSscType) @WalletRealWebMode)
        (liftWalletRedirects . lift . lift)
  where
    unwrapWPMode = runWalletWebDB db . runWalletWS conn . runWalletRedirects
{-# NOINLINE runWRealMode #-}

walletServeWebFull
    :: SscConstraint WalletSscType
    => SendActions WalletRealWebMode
    -> Bool      -- whether to include genesis keys
    -> Word16    -- ^ Port to listen
    -> FilePath  -- ^ TLS Certificate path
    -> FilePath  -- ^ TLS Key file
    -> FilePath  -- ^ TLS ca file
    -> WalletRealWebMode ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletRealWebMode Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when debug $ addInitialRichAccount 0
        walletApplication $ walletServer sendActions nat
