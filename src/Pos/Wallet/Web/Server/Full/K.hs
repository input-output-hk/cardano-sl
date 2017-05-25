{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for `RawRealModeK`-related part of full-node implementation of
-- Daedalus API

module Pos.Wallet.Web.Server.Full.K
       ( walletServeWebFull
       , runWStatsMode
       , runWProductionMode
       , WalletStatsMode
       , WalletProductionMode
       ) where

import           Mockable                          (Production)
import           Network.Transport.Abstract        (Transport)
import           Network.Wai                       (Application)
import qualified STMContainers.Map                 as SM
import           System.Wlog                       (logInfo)
import           Universum

import           Pos.Communication                 (ActionSpec (..), OutSpecs)
import           Pos.Communication.Protocol        (SendActions)
import           Pos.Constants                     (isDevelopment)
import           Pos.Crypto                        (noPassEncrypt)
import           Pos.DHT.Real                      (KademliaDHTInstance)
import           Pos.Genesis                       (genesisDevSecretKeys)
import           Pos.Launcher.Param                (NodeParams (..))
import           Pos.Launcher.Runner               (runRawKBasedMode)
import           Pos.Ssc.Class                     (SscConstraint, SscParams)
import           Pos.Statistics                    (NoStatsT, StatsT, getNoStatsT,
                                                    runStatsT')
import           Pos.Wallet.KeyStorage             (addSecretKey)
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.Web.Server.Full.Common (nat)
import           Pos.Wallet.Web.Server.Methods     (WalletWebHandler, walletApplication,
                                                    walletServeImpl, walletServer)
import           Pos.Wallet.Web.Server.Sockets     (ConnectionsVar, runWalletWS)
import           Pos.Wallet.Web.State              (WalletState, runWalletWebDB)
import           Pos.WorkMode                      (RawRealModeK)


type WalletProductionMode = NoStatsT $ WalletWebHandler (RawRealModeK WalletSscType)

type WalletStatsMode = StatsT $ WalletWebHandler (RawRealModeK WalletSscType)

-- | WalletProductionMode runner.
runWProductionMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> Transport WalletProductionMode
    -> KademliaDHTInstance
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletProductionMode a, OutSpecs)
    -> Production a
runWProductionMode db conn =
    runRawKBasedMode
        unwrapWPMode
        (lift . lift . lift)
  where
    unwrapWPMode = runWalletWebDB db . runWalletWS conn . getNoStatsT

-- | WalletProductionMode runner.
runWStatsMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> Transport WalletStatsMode
    -> KademliaDHTInstance
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletStatsMode a, OutSpecs)
    -> Production a
runWStatsMode db conn transport kinst param sscp runAction = do
    statMap <- liftIO SM.newIO
    runRawKBasedMode
        (unwrapWSMode statMap)
        (lift . lift . lift)
        transport
        kinst
        param
        sscp
        runAction
  where
    unwrapWSMode statMap = runWalletWebDB db . runWalletWS conn . runStatsT' statMap

walletServeWebFull
    :: SscConstraint WalletSscType
    => SendActions (WalletWebHandler (RawRealModeK WalletSscType))
    -> Bool      -- whether to include genesis keys
    -> Word16
    -> WalletWebHandler (RawRealModeK WalletSscType) ()
walletServeWebFull sendActions debug = walletServeImpl action
  where
    action :: WalletWebHandler (RawRealModeK WalletSscType) Application
    action = do
        logInfo "DAEDALUS has STARTED!"
        when (isDevelopment && debug) $
            mapM_ (addSecretKey . noPassEncrypt) genesisDevSecretKeys
        walletApplication $ walletServer sendActions nat
