{-# LANGUAGE MultiParamTypeClasses #-}
module Pos.DHT.Workers
       ( dhtWorkers
       ) where

import           Data.Binary           (encode)
import qualified Data.ByteString.Lazy  as BS
import           Formatting            (sformat, (%))
import           Network.Kademlia      (takeSnapshot)
import           Node                  (SendActions)
import           System.Wlog           (logNotice)
import           Universum

import qualified Pos.Binary.Class      as BI
import           Pos.Binary.DHTModel   ()
import           Pos.Constants         (epochSlots)
import           Pos.Context           (getNodeContext, ncKademliaDump)
import           Pos.Communication.BiP (BiP)
import           Pos.DHT.Model.Types   (DHTKey)
import           Pos.DHT.Real.Types    (KademliaDHTInstance (..),
                                        WithKademliaDHTInstance (..))
import           Pos.Slotting          (onNewSlot)
import           Pos.Types             (slotIdF)
import           Pos.Types.Slotting    (flattenSlotId)
import           Pos.WorkMode          (WorkMode)

dumpKademliaStateInterval :: Integral a => a
dumpKademliaStateInterval = epochSlots

dhtWorkers :: WorkMode ssc m => [SendActions BiP m -> m ()]
dhtWorkers = [dumpKademliaStateWorker]

dumpKademliaStateWorker :: WorkMode ssc m => SendActions BiP m -> m ()
dumpKademliaStateWorker __sendActions = onNewSlot True $ \slotId -> do
    when (flattenSlotId slotId `mod` dumpKademliaStateInterval == 0) $ do
        dumpFile <- ncKademliaDump <$> getNodeContext
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        inst <- kdiHandle <$> getKademliaDHTInstance
        snapshot <- liftIO $ takeSnapshot inst
        liftIO . BS.writeFile dumpFile $ encode snapshot
