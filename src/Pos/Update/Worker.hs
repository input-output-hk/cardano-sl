-- | Update System related workers.

module Pos.Update.Worker
       ( usWorkers
       ) where

import           Node                  (SendActions)
import           Universum

import           Pos.Communication.BiP (BiP)
import           Pos.Slotting          (onNewSlot')
import           Pos.Types             (SlotId)
import           Pos.WorkMode          (NewWorkMode)

-- | Update System related workers.
usWorkers :: NewWorkMode ssc m => [SendActions BiP m -> m ()]
usWorkers = [const usOnNewSlot]

usOnNewSlot :: NewWorkMode ssc m => m ()
usOnNewSlot = onNewSlot' True onNewSlotAction

onNewSlotAction :: NewWorkMode ssc m => SlotId -> m ()
onNewSlotAction _ = pass
