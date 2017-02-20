-- | High level workers.

module Pos.Worker
       ( allWorkers
       , allWorkersCount
       ) where

import           Data.Tagged           (untag)
import           Universum

import           Pos.Block.Worker      (blkWorkers)
import           Pos.Communication     (OutSpecs, WorkerSpec, localWorker, relayWorkers,
                                        wrapActionSpec)
import           Pos.Delegation.Worker (dlgWorkers)
import           Pos.DHT.Workers       (dhtWorkers)
import           Pos.Lrc.Worker        (lrcOnNewSlotWorker)
import           Pos.Security.Workers  (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting.Class    (MonadSlots (slottingWorkers))
import           Pos.Slotting.Util     (logNewSlotWorker)
import           Pos.Ssc.Class.Workers (SscWorkersClass, sscWorkers)
import           Pos.Ssc.GodTossing    (SscGodTossing)
import           Pos.Update            (usWorkers)
import           Pos.Util              (mconcatPair)
import           Pos.Worker.SysStart   (sysStartWorker)
import           Pos.WorkMode          (ProductionMode, WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: (SscWorkersClass ssc, SecurityWorkersClass ssc, WorkMode ssc m)
    => ([WorkerSpec m], OutSpecs)
allWorkers = mconcatPair
    [
      -- Only workers of "onNewSlot" type
      wrap' "dht"        $ dhtWorkers
    , wrap' "ssc"        $ untag sscWorkers
    , wrap' "security"   $ untag securityWorkers
    , wrap' "lrc"        $ first pure lrcOnNewSlotWorker
    , wrap' "us"         $ usWorkers
    , wrap' "sysStart"   $ first pure sysStartWorker

      -- Have custom loggers
    , wrap' "block"      $ blkWorkers
    , wrap' "delegation" $ dlgWorkers
    , wrap' "slotting"   $ (properSlottingWorkers, mempty)
    , wrap' "relay"      $ relayWorkers

    -- I don't know, guys, I don't know :(
    -- , const ([], mempty) statsWorkers
    ]
  where
    properSlottingWorkers =
        map (fst . localWorker) (logNewSlotWorker:slottingWorkers)
    wrap' lname = first (map $ wrapActionSpec $ "worker" <> lname)

allWorkersCount :: Int
allWorkersCount =
    length $ fst (allWorkers @SscGodTossing @(ProductionMode SscGodTossing))
