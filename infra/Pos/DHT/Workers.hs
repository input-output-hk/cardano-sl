{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Data.Binary                (encode)
import qualified Data.ByteString.Lazy       as BS
import           Formatting                 (sformat, (%))
import           Mockable                   (Delay, Fork, Mockable)
import           Network.Kademlia           (takeSnapshot)
import           System.Wlog                (WithLogger, logNotice)
import           Universum

import           Pos.Binary.Infra.DHTModel  ()
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker)
import           Pos.Core.Slotting          (flattenSlotId)
import           Pos.Core.Types             (slotIdF)
import           Pos.DHT.Constants          (kademliaDumpInterval)
import           Pos.DHT.Real.Types         (KademliaDHTInstance (..))
import           Pos.Discovery.Class        (MonadDiscovery)
import           Pos.Reporting              (HasReportingContext)
import           Pos.Shutdown               (MonadShutdownMem)
import           Pos.Slotting.Class         (MonadSlots)

type DhtWorkMode ctx m =
    ( WithLogger m
    , MonadSlots m
    , MonadIO m
    , MonadMask m
    , Mockable Fork m
    , Mockable Delay m
    , MonadReader ctx m
    , HasReportingContext ctx
    , MonadShutdownMem ctx m
    , MonadDiscovery m
    )

dhtWorkers
    :: DhtWorkMode ctx m
    => KademliaDHTInstance -> ([WorkerSpec m], OutSpecs)
dhtWorkers kademliaInst = first pure (dumpKademliaStateWorker kademliaInst)

dumpKademliaStateWorker
    :: DhtWorkMode ctx m
    => KademliaDHTInstance
    -> (WorkerSpec m, OutSpecs)
dumpKademliaStateWorker kademliaInst = localOnNewSlotWorker True $ \slotId ->
    when (flattenSlotId slotId `mod` kademliaDumpInterval == 0) $ do
        let dumpFile = kdiDumpPath kademliaInst
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        let inst = kdiHandle kademliaInst
        snapshot <- liftIO $ takeSnapshot inst
        liftIO . BS.writeFile dumpFile $ encode snapshot
