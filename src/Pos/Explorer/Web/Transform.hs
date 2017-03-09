{-# LANGUAGE TypeOperators #-}

module Pos.Explorer.Web.Transform
       ( explorerServeWebReal
       , explorerPlugin
       , notifierPlugin
       ) where

import           Control.Concurrent.STM       (TVar)
import qualified Control.Monad.Catch          as Catch (Handler (..), catches)
import           Control.Monad.Except         (MonadError (throwError))
import           Mockable                     (runProduction)
import           Servant.Server               (Handler)
import           Servant.Utils.Enter          ((:~>) (..), enter)
import           System.Wlog                  (usingLoggerName)
import           Universum

import           Pos.Communication            (OutSpecs, PeerStateSnapshot, SendActions,
                                               WithPeerState (..), WorkerSpec,
                                               getAllStates, peerStateFromSnapshot,
                                               runPeerStateHolder, worker)
import           Pos.Context                  (NodeContext, getNodeContext,
                                               runContextHolder)
import           Pos.DB                       (NodeDBs, getNodeDBs, runDBHolder)
import           Pos.Delegation               (DelegationWrap, askDelegationState,
                                               runDelegationTFromTVar)
import           Pos.DHT.Real.Real            (runKademliaDHT)
import           Pos.DHT.Real.Types           (KademliaDHTInstance (..),
                                               getKademliaDHTInstance)
import           Pos.Slotting                 (NtpSlotting (..), NtpSlottingVar,
                                               SlottingHolder (..), SlottingVar,
                                               runNtpSlotting, runSlottingHolder)
import           Pos.Ssc.Extra                (SscHolder (..), SscState, runSscHolder)
import           Pos.Ssc.GodTossing           (SscGodTossing)
import           Pos.Statistics               (getNoStatsT)
import           Pos.Txp                      (TxpLocalData, askTxpMem,
                                               runTxpHolderReader)
import           Pos.Update                   (runUSHolder)
import           Pos.WorkMode                 (ProductionMode)

import           Pos.Explorer.Web.Server      (explorerApp, explorerHandlers,
                                               explorerServeImpl)
import           Pos.Explorer.Web.Sockets.App (NotifierSettings, notifierApp)

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
    kinst      <- lift getKademliaDHTInstance
    tlw        <- askTxpMem
    ssc        <- lift . lift . lift . lift . lift . lift $ SscHolder ask
    delWrap    <- askDelegationState
    psCtx      <- getAllStates
    nc         <- getNodeContext
    modernDB   <- getNodeDBs
    slotVar    <- lift . lift . lift . lift . lift . lift . lift . lift $ SlottingHolder ask
    ntpSlotVar <- lift . lift . lift . lift . lift . lift . lift $ NtpSlotting ask
    pure $ Nat (convertHandler kinst nc modernDB tlw ssc delWrap psCtx slotVar ntpSlotVar)

convertHandler
    :: KademliaDHTInstance
    -> NodeContext SscGodTossing
    -> NodeDBs
    -> TxpLocalData
    -> SscState SscGodTossing
    -> TVar DelegationWrap
    -> PeerStateSnapshot
    -> SlottingVar
    -> NtpSlottingVar
    -> ExplorerProd a
    -> Handler a
convertHandler kinst nc modernDBs tlw ssc delWrap psCtx slotVar ntpSlotVar handler =
    liftIO ( runProduction
           . usingLoggerName "explorer-api"
           . runDBHolder modernDBs
           . runContextHolder nc
           . runSlottingHolder slotVar
           . runNtpSlotting ntpSlotVar
           . runSscHolder ssc
           . runTxpHolderReader tlw
           . runDelegationTFromTVar delWrap
           . runUSHolder
           . runKademliaDHT kinst
           . (\m -> flip runPeerStateHolder m =<< peerStateFromSnapshot psCtx)
           . getNoStatsT
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
