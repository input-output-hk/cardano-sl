module Pos.DHT.Workers
       ( dhtWorkers
       ) where

import           Formatting         (sformat, (%))
import           Network.Kademlia   (takeSnapshot)
import           Pos.Constants      (epochSlots)
import           Pos.DHT.Real.Types (KademliaDHTInstance (..),
                                     WithKademliaDHTInstance (..))
import           Pos.Slotting       (onNewSlot)
import           Pos.Types          (slotIdF)
import           Pos.Types.Slotting (flattenSlotId)
import           Pos.WorkMode       (WorkMode)
import           System.Wlog        (logNotice)

import           Universum

dumpKademliaStateInterval :: Integral a => a
dumpKademliaStateInterval = epochSlots

dhtWorkers :: WorkMode ssc m => [m ()]
dhtWorkers = [dumpKademliaStateWorker]

dumpKademliaStateWorker :: WorkMode ssc m => m ()
dumpKademliaStateWorker = onNewSlot True $ \slotId -> do
    when (flattenSlotId slotId `mod` dumpKademliaStateInterval == 0) $ do
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        inst <- kdiHandle <$> getKademliaDHTInstance
        _snapshot <- liftIO $ takeSnapshot inst
        -- TODO: Here we should put kademlia snapshot into database
        pure ()
