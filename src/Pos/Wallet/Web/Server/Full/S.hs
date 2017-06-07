{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `StaticMode`-related part of full-node implementation of
-- Daedalus API. FYI: S stays for Static.

module Pos.Wallet.Web.Server.Full.S
       ( walletServeWebFullS
       , runWStaticMode
       , WalletStaticMode
       ) where

import           Mockable                          (Production)
import           Network.Transport.Abstract        (Transport)
import           Network.Wai                       (Application)
import           System.Wlog                       (logInfo)
import           Universum

import           Pos.Communication                 (ActionSpec (..), NodeId, OutSpecs)
import           Pos.Communication.Protocol        (SendActions)
import           Pos.Constants                     (isDevelopment)
import           Pos.Crypto                        (noPassEncrypt)
import           Pos.Genesis                       (genesisDevSecretKeys)
import           Pos.Launcher.Param                (NodeParams (..))
import           Pos.Launcher.Runner               (runStaticBasedMode)
import           Pos.Ssc.Class                     (SscConstraint, SscParams)
import           Pos.Wallet.KeyStorage             (addSecretKey)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Server.Full.Common (natS)
import           Pos.Wallet.Web.Server.Methods     (WalletWebHandler, walletApplication,
                                                    walletServeImpl, walletServer)
import           Pos.Wallet.Web.Server.Sockets     (ConnectionsVar, runWalletWS)
import           Pos.Wallet.Web.State              (WalletState, runWalletWebDB)
import           Pos.WorkMode                      (StaticMode)

type WalletStaticMode = WalletWebHandler (StaticMode WalletSscType)

-- | WalletProductionMode runner.
runWStaticMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> Set NodeId
    -> Transport WalletStaticMode
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletStaticMode a, OutSpecs)
    -> Production a
runWStaticMode db conn =
    runStaticBasedMode (runWalletWebDB db . runWalletWS conn) (lift . lift)
{-# NOINLINE runWStaticMode #-}

walletServeWebFullS
    :: SscConstraint WalletSscType
    => SendActions WalletStaticMode
    -> Bool      -- whether to include genesis keys
    -> Word16
    -> WalletStaticMode ()
walletServeWebFullS sendActions debug = walletServeImpl action
  where
    action :: WalletStaticMode Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when (isDevelopment && debug) $
            mapM_ (addSecretKey . noPassEncrypt) genesisDevSecretKeys
        walletApplication $ walletServer sendActions natS
