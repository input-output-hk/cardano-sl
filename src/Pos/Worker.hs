-- | High level workers.

module Pos.Worker
       ( runWorkers
       ) where

import           Control.TimeWarp.Logging (logInfo)
import           Control.TimeWarp.Timed   (fork_, repeatForever)
import           Formatting               (build, sformat, (%))
import           Universum

import           Pos.DHT                  (DHTNodeType (..), discoverPeers)
import           Pos.Slotting             (onNewSlot)
import           Pos.Types                (SlotId, slotIdF)
import           Pos.Worker.Block         (blkOnNewSlot, blkWorkers)
import           Pos.WorkMode             (WorkMode)

-- | Run all necessary workers in separate threads. This call doesn't
-- block.
runWorkers :: WorkMode m => m ()
runWorkers = mapM_ fork_ (testPeerDiscoverWorker : onNewSlotWorker : blkWorkers)

onNewSlotWorker :: WorkMode m => m ()
onNewSlotWorker = onNewSlot False onNewSlotWorkerImpl

onNewSlotWorkerImpl :: WorkMode m => SlotId -> m ()
onNewSlotWorkerImpl slotId = do
    logInfo $ sformat ("New slot has just started: "%slotIdF) slotId
    blkOnNewSlot slotId

testPeerDiscoverWorker :: WorkMode m => m ()
testPeerDiscoverWorker = do
  repeatForever 1000000 (const $ return 1000000) $ do
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers
