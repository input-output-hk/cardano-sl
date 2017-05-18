{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for full-node implementation of Daedalus API

module Pos.Wallet.Web.Server.Full
       ( walletServeWebFull
       , walletServerOuts
       , liftWMode
       , runWStatsMode
       , runWProductionMode
       , WalletStatsMode
       , WalletProductionMode
       ) where

import           Universum

import           Control.Concurrent.STM        (TVar)
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Control.Monad.Trans           (MonadTrans)
import           Data.Tagged                   (Tagged (..))
import qualified Ether
import           Mockable                      (Production, runProduction)
import           Network.Transport.Abstract    (Transport)
import           Network.Wai                   (Application)
import           Pos.Slotting.Ntp              (runSlotsRedirect)
import           Pos.Ssc.Extra.Class           (askSscMem)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import qualified STMContainers.Map             as SM
import           System.Wlog                   (logInfo, usingLoggerName)

import           Pos.Block.BListener           (runBListenerStub)
import           Pos.Client.Txp.Balances       (runBalancesRedirect)
import           Pos.Client.Txp.History        (runTxHistoryRedirect)
import           Pos.Communication             (ActionSpec (..), OutSpecs, PeerId)
import           Pos.Communication.PeerState   (PeerStateSnapshot, PeerStateTag,
                                                WithPeerState (..), getAllStates,
                                                peerStateFromSnapshot,
                                                runPeerStateRedirect)
import           Pos.Communication.Protocol    (SendActions)
import           Pos.Constants                 (isDevelopment)
import           Pos.Context                   (NodeContext, NodeContextTag)
import           Pos.Crypto                    (noPassEncrypt)
import           Pos.DB                        (NodeDBs, getNodeDBs)
import           Pos.DB.DB                     (runDbCoreRedirect)
import           Pos.Delegation.Class          (DelegationWrap, askDelegationState)
import           Pos.DHT.Real                  (KademliaDHTInstance)
import           Pos.Discovery                 (askDHTInstance, runDiscoveryKademliaT)
import           Pos.Genesis                   (genesisDevSecretKeys)
import           Pos.Launcher.Param            (NodeParams (..))
import           Pos.Launcher.Runner           (runRawKBasedMode)
import           Pos.Slotting                  (NtpSlottingVar, SlottingVar,
                                                askFullNtpSlotting, askSlotting,
                                                runSlotsDataRedirect)
import           Pos.Ssc.Class                 (SscConstraint, SscParams)
import           Pos.Ssc.Extra                 (SscMemTag, SscState)
import           Pos.Statistics                (NoStatsT, StatsMap, StatsT, getNoStatsT,
                                                runStatsT')
import           Pos.Txp                       (GenericTxpLocalData, TxpHolderTag,
                                                askTxpMem)
import           Pos.Update.DB                 (runDbLimitsRedirect)
import           Pos.Wallet.KeyStorage         (addSecretKey)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.WalletMode         (runBlockchainInfoRedirect,
                                                runUpdatesRedirect)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler, walletApplication,
                                                walletServeImpl, walletServer,
                                                walletServerOuts)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, getWalletWebSockets,
                                                runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, getWalletWebState,
                                                runWalletWebDB)
import           Pos.WorkMode                  (RawRealModeK, TxpExtra_TMP)


----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

type WalletProductionMode = NoStatsT $ WalletWebHandler (RawRealModeK WalletSscType)

type WalletStatsMode = StatsT $ WalletWebHandler (RawRealModeK WalletSscType)

liftWMode
    :: ( Each '[MonadTrans] [t1, t2, t3]
       , Each '[Monad] [m, t3 m, t2 (t3 m)]
       )
    => m a -> (t1 $ t2 $ t3 m) a
liftWMode = lift . lift . lift

unwrapWPMode
    :: WalletState
    -> ConnectionsVar
    -> WalletProductionMode a
    -> RawRealModeK WalletSscType a
unwrapWPMode db conn = runWalletWebDB db . runWalletWS conn . getNoStatsT

unwrapWSMode
    :: WalletState
    -> ConnectionsVar
    -> StatsMap
    -> WalletStatsMode a
    -> RawRealModeK WalletSscType a
unwrapWSMode db conn statMap = runWalletWebDB db . runWalletWS conn . runStatsT' statMap

-- | WalletProductionMode runner.
runWProductionMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> PeerId
    -> Transport WalletProductionMode
    -> KademliaDHTInstance
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletProductionMode a, OutSpecs)
    -> Production a
runWProductionMode db conn = runRawKBasedMode (unwrapWPMode db conn) liftWMode

-- | WalletProductionMode runner.
runWStatsMode
    :: SscConstraint WalletSscType
    => WalletState
    -> ConnectionsVar
    -> PeerId
    -> Transport WalletStatsMode
    -> KademliaDHTInstance
    -> NodeParams
    -> SscParams WalletSscType
    -> (ActionSpec WalletStatsMode a, OutSpecs)
    -> Production a
runWStatsMode db conn peer transport kinst param sscp runAction = do
    statMap <- liftIO SM.newIO
    runRawKBasedMode
        (unwrapWSMode db conn statMap)
        liftWMode
        peer
        transport
        kinst
        param
        sscp
        runAction

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

type WebHandler = WalletWebHandler (RawRealModeK WalletSscType)

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    ws         <- getWalletWebState
    tlw        <- askTxpMem
    ssc        <- askSscMem
    delWrap    <- askDelegationState
    psCtx      <- getAllStates
    nc         <- Ether.ask @NodeContextTag
    modernDB   <- getNodeDBs
    conn       <- getWalletWebSockets
    slotVar    <- askSlotting
    ntpSlotVar <- askFullNtpSlotting
    kinst      <- askDHTInstance
    pure $ NT (convertHandler nc modernDB tlw ssc ws delWrap
                              psCtx conn slotVar ntpSlotVar kinst)

convertHandler
    :: NodeContext WalletSscType              -- (.. insert monad `m` here ..)
    -> NodeDBs
    -> GenericTxpLocalData TxpExtra_TMP
    -> SscState WalletSscType
    -> WalletState
    -> (TVar DelegationWrap)
    -> PeerStateSnapshot
    -> ConnectionsVar
    -> SlottingVar
    -> (Bool, NtpSlottingVar)
    -> KademliaDHTInstance
    -> WebHandler a
    -> Handler a
convertHandler nc modernDBs tlw ssc ws delWrap psCtx
               conn slotVar ntpSlotVar kinst handler = do
    liftIO ( runProduction
           . usingLoggerName "wallet-api"
           . flip Ether.runReadersT nc
           . (\m -> do
               peerStateCtx <- peerStateFromSnapshot psCtx
               Ether.runReadersT m
                   ( Tagged @NodeDBs modernDBs
                   , Tagged @SlottingVar slotVar
                   , Tagged @(Bool, NtpSlottingVar) ntpSlotVar
                   , Tagged @SscMemTag ssc
                   , Tagged @TxpHolderTag tlw
                   , Tagged @(TVar DelegationWrap) delWrap
                   , Tagged @PeerStateTag peerStateCtx
                   ))
           . runSlotsDataRedirect
           . runSlotsRedirect
           . runBalancesRedirect
           . runTxHistoryRedirect
           . runPeerStateRedirect
           . runDbLimitsRedirect
           . runDbCoreRedirect
           . runUpdatesRedirect
           . runBlockchainInfoRedirect
           . runBListenerStub
           . runDiscoveryKademliaT kinst
           . runWalletWebDB ws
           . runWalletWS conn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
