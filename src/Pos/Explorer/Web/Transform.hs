{-# LANGUAGE TypeOperators #-}

module Pos.Explorer.Web.Transform
       ( explorerServeWebReal
       , explorerPlugin
       ) where

import           Control.Concurrent.STM  (TVar)
import qualified Control.Monad.Catch     as Catch (Handler (..), catches)
import           Control.Monad.Except    (MonadError (throwError))
import           Mockable                (runProduction)
import           Servant.Server          (Handler)
import           Servant.Utils.Enter     ((:~>) (..), enter)
import           System.Wlog             (usingLoggerName)
import           Universum

import           Pos.Communication       (OutSpecs, PeerStateSnapshot, SendActions,
                                          WithPeerState (..), WorkerSpec, getAllStates,
                                          peerStateFromSnapshot, runPeerStateHolder,
                                          worker)
import           Pos.Context             (NodeContext, getNodeContext, runContextHolder)
import           Pos.DB                  (NodeDBs, getNodeDBs, runDBHolder)
import           Pos.Delegation          (DelegationWrap, askDelegationState,
                                          runDelegationTFromTVar)
import           Pos.DHT.Real.Real       (runKademliaDHT)
import           Pos.DHT.Real.Types      (KademliaDHTInstance (..),
                                          getKademliaDHTInstance)
import           Pos.Ssc.Extra           (SscHolder (..), SscState, runSscHolderRaw)
import           Pos.Ssc.GodTossing      (SscGodTossing)
import           Pos.Statistics          (getNoStatsT)
import           Pos.Txp                 (TxpLDWrap, getTxpLDWrap, runTxpLDHolderReader)
import           Pos.Update              (runUSHolder)
import           Pos.WorkMode            (ProductionMode)

import           Pos.Explorer.Web.Server (explorerApp, explorerHandlers,
                                          explorerServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type ExplorerProd = ProductionMode SscGodTossing

explorerPlugin :: Word16 -> ([WorkerSpec ExplorerProd], OutSpecs)
explorerPlugin = first pure . worker mempty . flip explorerServeWebReal

explorerServeWebReal :: SendActions ExplorerProd -> Word16 -> ExplorerProd ()
explorerServeWebReal sendActions = explorerServeImpl . explorerApp $
    flip enter (explorerHandlers sendActions) <$> nat

nat :: ExplorerProd (ExplorerProd :~> Handler)
nat = do
    kinst    <- lift getKademliaDHTInstance
    tlw      <- getTxpLDWrap
    ssc      <- lift . lift . lift . lift . lift . lift $ SscHolder ask
    delWrap  <- askDelegationState
    psCtx    <- getAllStates
    nc       <- getNodeContext
    modernDB <- getNodeDBs
    pure $ Nat (convertHandler kinst nc modernDB tlw ssc delWrap psCtx)

convertHandler
    :: KademliaDHTInstance
    -> NodeContext SscGodTossing
    -> NodeDBs SscGodTossing
    -> TxpLDWrap SscGodTossing
    -> SscState SscGodTossing
    -> TVar DelegationWrap
    -> PeerStateSnapshot
    -> ExplorerProd a
    -> Handler a
convertHandler kinst nc modernDBs tlw ssc delWrap psCtx handler =
    liftIO ( runProduction
           . usingLoggerName "explorer-api"
           . runDBHolder modernDBs
           . runContextHolder nc
           . runSscHolderRaw ssc
           . runTxpLDHolderReader tlw
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
