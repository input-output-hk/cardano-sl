-- | Slotting workers.

module Pos.Slotting.Worker
       ( slottingWorker
       ) where

import           Formatting        (sformat, (%))
import           System.Wlog       (logNotice, modifyLoggerName)
import           Universum

import           Pos.Communication (OutSpecs, WorkerSpec, onNewSlotWithLoggingWorker)
import           Pos.Context       (setNtpLastSlot)
import           Pos.Types         (slotIdF)
import           Pos.WorkMode      (WorkMode)

-- | This worker is necessary for Slotting to work.
slottingWorker
    :: WorkMode ssc m
    => (WorkerSpec m, OutSpecs)
slottingWorker =
    onNewSlotWithLoggingWorker True mempty $ \slotId _ -> do
        modifyLoggerName (<> "slotting") $
            logNotice $ sformat ("New slot has just started: " %slotIdF) slotId
        setNtpLastSlot slotId
