{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `ProductionMode`-related part of full-node implementation of
-- Daedalus API. FYI: K stays for Kademlia.

module Pos.Wallet.Web.Server.Full.K
       ( walletServeWebFull
       , runWProductionMode
       , WalletProductionMode
       ) where

import           Universum

import           Mockable                          (Production)
import           Network.Transport.Abstract        (Transport)
import           Network.Wai                       (Application)
import           System.Wlog                       (logInfo)

import           Pos.Communication                 (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol        (SendActions)
import           Pos.Constants                     (isDevelopment)
import           Pos.Crypto                        (noPassEncrypt)
import           Pos.DHT.Real                      (KademliaDHTInstance)
import           Pos.Genesis                       (genesisDevSecretKeys)
import           Pos.Launcher.Param                (NodeParams (..))
import           Pos.Launcher.Runner               (runProductionBasedMode)
import           Pos.Ssc.Class                     (SscConstraint, SscParams)
import           Pos.Wallet.KeyStorage             (addSecretKey)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Server.Full.Common (nat)
import           Pos.Wallet.Web.Server.Methods     (WalletWebHandler, walletApplication,
                                                    walletServeImpl, walletServer)
import           Pos.Wallet.Web.Server.Sockets     (ConnectionsVar, runWalletWS)
import           Pos.Wallet.Web.State              (WalletState, runWalletWebDB)
import           Pos.WorkMode                      (ProductionMode)

type WalletProductionMode = WalletWebHandler (ProductionMode WalletSscType)

-- | WalletProductionMode runner.
runWProductionMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> KademliaDHTInstance
    -> Transport WalletProductionMode
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletProductionMode a, OutSpecs)
    -> Production a
runWProductionMode db conn =
    runProductionBasedMode
        unwrapWPMode
        (lift . lift)
  where
    unwrapWPMode = runWalletWebDB db . runWalletWS conn
{-# NOINLINE runWProductionMode #-}

walletServeWebFull
    :: SscConstraint WalletSscType
    => SendActions WalletProductionMode
    -> Bool      -- whether to include genesis keys
    -> Word16
    -> WalletProductionMode ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletProductionMode Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when (isDevelopment && debug) $
            mapM_ (addSecretKey . noPassEncrypt) genesisDevSecretKeys
        walletApplication $ walletServer sendActions nat
