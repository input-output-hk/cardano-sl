{-# LANGUAGE MultiParamTypeClasses #-}
module Pos.DHT.Workers
       ( dhtWorkers
       ) where

import           Data.Binary                (encode)
import qualified Data.ByteString.Lazy       as BS
import           Formatting                 (sformat, (%))
import           Network.Kademlia           (takeSnapshot)
import           System.Wlog                (logNotice)
import           Universum

import           Pos.Binary.Class           (Bi)
import           Pos.Binary.DHTModel        ()
import           Pos.Communication.Protocol (Worker, worker)
import           Pos.Constants              (kademliaDumpInterval)
import           Pos.Context                (getNodeContext, ncKademliaDump)
import           Pos.DHT.Real.Types         (KademliaDHTInstance (..),
                                             WithKademliaDHTInstance (..))
import           Pos.Slotting               (onNewSlot)
import           Pos.Types                  (SlotId, slotIdF)
import           Pos.Types.Slotting         (flattenSlotId)
import           Pos.WorkMode               (WorkMode)

dhtWorkers :: (WorkMode ssc m) => [Worker m]
dhtWorkers = [onNewSlot True $ dumpKademliaStateWorker]

dumpKademliaStateWorker slotId = worker . const $ do
    when (flattenSlotId slotId `mod` kademliaDumpInterval == 0) $ do
        dumpFile <- ncKademliaDump <$> getNodeContext
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        inst <- kdiHandle <$> getKademliaDHTInstance
        snapshot <- liftIO $ takeSnapshot inst
        liftIO . BS.writeFile dumpFile $ encode snapshot
