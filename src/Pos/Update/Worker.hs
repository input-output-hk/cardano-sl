-- | Update System related workers.

module Pos.Update.Worker
       ( usWorkers
       ) where

import           Universum

import           Pos.Communication.Protocol (Worker, worker)
import           Pos.Slotting               (onNewSlot)
import           Pos.Types                  (SlotId)
import           Pos.Update.Logic.Local     (processNewSlot)
import           Pos.WorkMode               (WorkMode)

-- | Update System related workers.
usWorkers :: WorkMode ssc m => [Worker m]
usWorkers = [onNewSlot True $ \s -> worker (const $ processNewSlot s) ]
