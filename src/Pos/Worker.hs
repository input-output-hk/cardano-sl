-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Data.Tagged           (untag)
import           Universum

import           Pos.Block.Worker      (blkWorkers)
import           Pos.Communication     (OutSpecs, WorkerSpec)
import           Pos.Delegation.Worker (dlgWorkers)
import           Pos.DHT.Workers       (dhtWorkers)
import           Pos.Lrc.Worker        (lrcOnNewSlotWorker)
import           Pos.Security.Workers  (SecurityWorkersClass, securityWorkers)
-- import           Pos.Slotting          (slottingWorker)
import           Pos.Ssc.Class.Workers (SscWorkersClass, sscWorkers)
import           Pos.Update            (usWorkers)
import           Pos.Util              (mconcatPair)
import           Pos.Worker.SysStart   (sysStartWorker)
import           Pos.WorkMode          (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: (SscWorkersClass ssc, SecurityWorkersClass ssc, WorkMode ssc m)
    => ([WorkerSpec m], OutSpecs)
allWorkers = mconcatPair
    [ dhtWorkers
    , blkWorkers
    , dlgWorkers
    , untag sscWorkers
    , untag securityWorkers
    , first pure lrcOnNewSlotWorker
    , usWorkers
    , undefined -- first pure slottingWorker
    , first pure sysStartWorker
    -- I don't know, guys, I don't know :(
    -- , const ([], mempty) statsWorkers
    ]
