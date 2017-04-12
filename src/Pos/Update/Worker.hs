-- | Update System related workers.

module Pos.Update.Worker
       ( usWorkers
       ) where

import           Mockable                   (fork)
import           Universum

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker,
                                             NodeId)
import           Pos.Constants              (curSoftwareVersion)
import           Pos.Types                  (SoftwareVersion (..))
import           Pos.Update.DB              (getConfirmedProposals)
import           Pos.Update.Download        (downloadUpdate)
import           Pos.Update.Logic.Local     (processNewSlot)
import           Pos.WorkMode               (WorkMode)

-- | Update System related workers.
usWorkers :: WorkMode ssc m => m (Set NodeId) -> ([WorkerSpec m], OutSpecs)
usWorkers getPeers =
    first pure $
    localOnNewSlotWorker getPeers True $ \s ->
        processNewSlot s >> void (fork checkForUpdate)

checkForUpdate :: WorkMode ssc m => m ()
checkForUpdate =
    mapM_ downloadUpdate =<<
    getConfirmedProposals (Just $ svNumber curSoftwareVersion)
