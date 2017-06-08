{-# LANGUAGE TypeOperators #-}

module Pos.Explorer.Web.Transform
       ( explorerServeWebReal
       , explorerPlugin
       , notifierPlugin
       ) where

import qualified Control.Monad.Catch         as Catch (Handler (..), catches)
import           Control.Monad.Except        (MonadError (throwError))
import           Data.Tagged                 (Tagged (..))
import qualified Ether
import           Mockable                    (runProduction)
import           Servant.Server              (Handler)
import           Servant.Utils.Enter         ((:~>) (..), enter)
import           System.Wlog                 (usingLoggerName)
import           Universum

import           Pos.Block.BListener         (runBListenerStub)
import           Pos.Client.Txp.Balances     (runBalancesRedirect)
import           Pos.Client.Txp.History      (runTxHistoryRedirect)
import           Pos.Communication           (OutSpecs, PeerStateSnapshot, SendActions,
                                              WithPeerState (..), WorkerSpec,
                                              getAllStates, peerStateFromSnapshot, worker)
import           Pos.Communication.PeerState (PeerStateTag, runPeerStateRedirect)
import           Pos.Context                 (NodeContext, NodeContextTag)
import           Pos.DB                      (NodeDBs, getNodeDBs, runDBPureRedirect)
import           Pos.DB.Block                (runBlockDBRedirect)
import           Pos.DB.DB                   (runGStateCoreRedirect)
import           Pos.Delegation              (DelegationVar, askDelegationState)
import           Pos.DHT.Real                (KademliaDHTInstance)
import           Pos.Discovery               (askDHTInstance, runDiscoveryKademliaT)
import           Pos.Slotting                (NtpSlottingVar, SlottingVar,
                                              askFullNtpSlotting, askSlotting,
                                              runSlotsDataRedirect)
import           Pos.Slotting.Ntp            (runSlotsRedirect)
import           Pos.Ssc.Extra               (SscMemTag, SscState, askSscMem)
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Statistics              (getNoStatsT)
import           Pos.Txp                     (GenericTxpLocalData, TxpHolderTag,
                                              askTxpMem)
import           Pos.Wallet                  (runBlockchainInfoRedirect,
                                              runUpdatesRedirect)
import           Pos.WorkMode                (ProductionMode)

import           Pos.Explorer                (ExplorerExtra)
import           Pos.Explorer.Socket.App     (NotifierSettings, notifierApp)
import           Pos.Explorer.Web.Server     (explorerApp, explorerHandlers,
                                              explorerServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type ExplorerProd = ProductionMode SscGodTossing

notifierPlugin :: NotifierSettings -> ([WorkerSpec ExplorerProd], OutSpecs)
notifierPlugin = first pure . worker mempty .
    \settings _sa -> notifierApp @SscGodTossing settings

explorerPlugin :: Word16 -> ([WorkerSpec ExplorerProd], OutSpecs)
explorerPlugin = first pure . worker mempty . flip explorerServeWebReal

explorerServeWebReal :: SendActions ExplorerProd -> Word16 -> ExplorerProd ()
explorerServeWebReal sendActions = explorerServeImpl . explorerApp $
    flip enter (explorerHandlers sendActions) <$> nat

nat :: ExplorerProd (ExplorerProd :~> Handler)
nat = do
    kinst      <- askDHTInstance
    tlw        <- askTxpMem
    ssc        <- askSscMem
    delWrap    <- askDelegationState
    psCtx      <- getAllStates
    nc         <- Ether.ask @NodeContextTag
    modernDB   <- getNodeDBs
    slotVar    <- askSlotting
    ntpSlotVar <- askFullNtpSlotting
    pure $ NT (convertHandler kinst nc modernDB tlw ssc delWrap psCtx slotVar ntpSlotVar)

convertHandler
    :: KademliaDHTInstance
    -> NodeContext SscGodTossing
    -> NodeDBs
    -> GenericTxpLocalData ExplorerExtra
    -> SscState SscGodTossing
    -> DelegationVar
    -> PeerStateSnapshot
    -> SlottingVar
    -> (Bool, NtpSlottingVar)
    -> ExplorerProd a
    -> Handler a
convertHandler kinst nc modernDBs tlw ssc delWrap psCtx slotVar ntpSlotVar handler =
    liftIO (rawRunner . runDiscoveryKademliaT kinst . getNoStatsT $ handler) `Catch.catches` excHandlers
  where
    rawRunner = runProduction
           . usingLoggerName "explorer-api"
           . flip Ether.runReadersT nc
           . (\m -> do
               peerStateCtx <- peerStateFromSnapshot psCtx
               Ether.runReadersT m
                   ( Tagged @NodeDBs modernDBs
                   , Tagged @SlottingVar slotVar
                   , Tagged @(Bool, NtpSlottingVar) ntpSlotVar
                   , Tagged @SscMemTag ssc
                   , Tagged @TxpHolderTag tlw
                   , Tagged @DelegationVar delWrap
                   , Tagged @PeerStateTag peerStateCtx
                   ))
           . runDBPureRedirect
           . runBlockDBRedirect
           . runSlotsDataRedirect
           . runSlotsRedirect
           . runBalancesRedirect
           . runTxHistoryRedirect
           . runPeerStateRedirect
           . runGStateCoreRedirect
           . runUpdatesRedirect
           . runBlockchainInfoRedirect
           . runBListenerStub
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
