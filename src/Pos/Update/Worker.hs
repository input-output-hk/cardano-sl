-- | Update System related workers.

module Pos.Update.Worker
       ( usWorkers
       ) where

import           Universum

import           Pos.Slotting (onNewSlot)
import           Pos.Types    (SlotId)
import           Pos.WorkMode (WorkMode)

-- | Update System related workers.
usWorkers :: WorkMode ssc m => [m ()]
usWorkers = [usOnNewSlot]

usOnNewSlot :: WorkMode ssc m => m ()
usOnNewSlot = onNewSlot True onNewSlotAction

onNewSlotAction :: WorkMode ssc m => SlotId -> m ()
onNewSlotAction _ = pass
