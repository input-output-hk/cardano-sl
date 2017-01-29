-- | Update System related workers.

module Pos.Update.Worker
       ( usWorkers
       ) where

import           Mockable                   (fork)
import           Universum

import           Pos.Communication.Protocol (Worker, worker)
import           Pos.Constants              (curSoftwareVersion)
import           Pos.DB.GState              (getConfirmedProposals)
import           Pos.Slotting               (onNewSlot)
import           Pos.Types                  (SlotId, SoftwareVersion (..))
import           Pos.Update.Download        (downloadUpdate)
import           Pos.Update.Logic.Local     (processNewSlot)
import           Pos.WorkMode               (WorkMode)

-- | Update System related workers.
usWorkers :: WorkMode ssc m => [Worker m]
usWorkers =
    [ onNewSlot True $ \s ->
          worker (const $ processNewSlot s >> void (fork checkForUpdate))
    ]

checkForUpdate :: WorkMode ssc m => m ()
checkForUpdate =
    mapM_ downloadUpdate =<<
    getConfirmedProposals (Just $ svNumber curSoftwareVersion)
