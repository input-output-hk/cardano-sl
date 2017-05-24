{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RawRealModeS`-related part of full-node implementation of
-- Daedalus API

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
import           Pos.Launcher.Runner               (runRawSBasedMode)
import           Pos.Ssc.Class                     (SscConstraint, SscParams)
import           Pos.Statistics                    (NoStatsT, getNoStatsT)
import           Pos.Wallet.KeyStorage             (addSecretKey)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Server.Full.Common (natS)
import           Pos.Wallet.Web.Server.Methods     (WalletWebHandler, walletApplication,
                                                    walletServeImpl, walletServer)
import           Pos.Wallet.Web.Server.Sockets     (ConnectionsVar, runWalletWS)
import           Pos.Wallet.Web.State              (WalletState, runWalletWebDB)
import           Pos.WorkMode                      (RawRealModeS)


type WalletStaticMode = NoStatsT $ WalletWebHandler (RawRealModeS WalletSscType)

-- | WalletProductionMode runner.
runWStaticMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> Transport WalletStaticMode
    -> Set NodeId
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletStaticMode a, OutSpecs)
    -> Production a
runWStaticMode db conn =
    runRawSBasedMode (runWalletWebDB db . runWalletWS conn . getNoStatsT) (lift . lift . lift)

walletServeWebFullS
    :: SscConstraint WalletSscType
    => SendActions (WalletWebHandler (RawRealModeS WalletSscType))
    -> Bool      -- whether to include genesis keys
    -> Word16
    -> WalletWebHandler (RawRealModeS WalletSscType) ()
walletServeWebFullS sendActions debug = walletServeImpl action
  where
    action :: WalletWebHandler (RawRealModeS WalletSscType) Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when (isDevelopment && debug) $
            mapM_ (addSecretKey . noPassEncrypt) genesisDevSecretKeys
        walletApplication $ walletServer sendActions natS
