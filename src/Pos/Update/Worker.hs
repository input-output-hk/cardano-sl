-- | Update System related workers.

module Pos.Update.Worker
       ( usWorkers
       ) where

import           Mockable                   (fork)
import           Universum

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker)
import           Pos.Constants              (curSoftwareVersion)
import           Pos.Context                (recoveryCommGuard)
import           Pos.Types                  (SoftwareVersion (..))
import           Pos.Update.DB              (getConfirmedProposals)
import           Pos.Update.Download        (downloadUpdate)
import           Pos.Update.Logic.Local     (processNewSlot)
import           Pos.WorkMode.Class         (WorkMode)

-- | Update System related workers.
usWorkers :: WorkMode ssc ctx m => ([WorkerSpec m], OutSpecs)
usWorkers =
    first pure $
    recoveryCommGuard $ localOnNewSlotWorker True $ \s ->
        processNewSlot s >> void (fork checkForUpdate)

checkForUpdate :: WorkMode ssc ctx m => m ()
checkForUpdate =
    mapM_ downloadUpdate =<<
    getConfirmedProposals (Just $ svNumber curSoftwareVersion)
