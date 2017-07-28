{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Universum

import qualified Data.ByteString.Lazy       as BSL
import           Formatting                 (sformat, (%))
import           Mockable                   (Delay, Fork, Mockable)
import           Network.Kademlia           (takeSnapshot)
import           System.Wlog                (WithLogger, logNotice)

import           Pos.Binary.Infra.DHTModel  ()
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker)
import           Pos.Core.Slotting          (flattenSlotId)
import           Pos.Core.Types             (slotIdF)
import           Pos.DHT.Constants          (kademliaDumpInterval)
import           Pos.DHT.Real.Types         (KademliaDHTInstance (..))
import           Pos.Discovery.Class        (MonadDiscovery)
import           Pos.Recovery.Info          (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Reporting              (HasReportingContext)
import           Pos.Shutdown               (HasShutdownContext)
import           Pos.Slotting.Class         (MonadSlots)
import           Pos.Binary.Class           (serialize)

type DhtWorkMode ctx m =
    ( WithLogger m
    , MonadSlots m
    , MonadIO m
    , MonadMask m
    , Mockable Fork m
    , Mockable Delay m
    , MonadRecoveryInfo m
    , MonadReader ctx m
    , HasReportingContext ctx
    , HasShutdownContext ctx
    , MonadDiscovery m
    )

dhtWorkers
    :: ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance -> ([WorkerSpec m], OutSpecs)
dhtWorkers kademliaInst = first pure (dumpKademliaStateWorker kademliaInst)

dumpKademliaStateWorker
    :: ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance
    -> (WorkerSpec m, OutSpecs)
dumpKademliaStateWorker kademliaInst = localOnNewSlotWorker True $ \slotId ->
    when (isTimeToDump slotId) $ recoveryCommGuard $ do
        let dumpFile = kdiDumpPath kademliaInst
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        let inst = kdiHandle kademliaInst
        snapshot <- liftIO $ takeSnapshot inst
        liftIO . BSL.writeFile dumpFile . serialize $ snapshot
  where
    isTimeToDump slotId = flattenSlotId slotId `mod` kademliaDumpInterval == 0
