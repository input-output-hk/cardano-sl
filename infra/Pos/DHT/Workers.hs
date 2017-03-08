{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.DHT.Workers
       ( dhtWorkers
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
import           Pos.DHT.MemState           (DhtContext (..), MonadDhtMem (..))
import           Pos.DHT.Model.Class        (MonadDHT)
import           Pos.DHT.Real.Types         (KademliaDHTInstance (..),
                                             WithKademliaDHTInstance (..))
import           Pos.Reporting              (MonadReportingMem)
import           Pos.Shutdown               (MonadShutdownMem)
import           Pos.Slotting.Class         (MonadSlots)

type DhtWorkMode m = ( WithLogger m
                     , MonadDhtMem m
                     , WithKademliaDHTInstance m
                     , MonadSlots m
                     , MonadIO m
                     , MonadMask m
                     , Mockable Fork m
                     , Mockable Delay m
                     , MonadDHT m
                     , MonadReportingMem m
                     , MonadShutdownMem m)

dhtWorkers :: DhtWorkMode m => ([WorkerSpec m], OutSpecs)
dhtWorkers = first pure dumpKademliaStateWorker

dumpKademliaStateWorker :: DhtWorkMode m => (WorkerSpec m, OutSpecs)
dumpKademliaStateWorker = localOnNewSlotWorker True $ \slotId ->
    when (flattenSlotId slotId `mod` kademliaDumpInterval == 0) $ do
        dumpFile <- _dhtKademliadDump <$> askDhtMem
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        inst <- kdiHandle <$> getKademliaDHTInstance
        snapshot <- liftIO $ takeSnapshot inst
        liftIO . BS.writeFile dumpFile $ encode snapshot
