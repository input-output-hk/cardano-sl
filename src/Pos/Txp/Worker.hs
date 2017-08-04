module Pos.Txp.Worker
       ( txpWorkers
       ) where

import           Universum

import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Ssc.Class       (SscWorkersClass)
import           Pos.Util            (mconcatPair)
import           Pos.WorkMode.Class  (WorkMode)

-- | All workers specific to transaction processing.
txpWorkers
    :: (SscWorkersClass ssc, WorkMode ssc ctx m)
    => ([WorkerSpec m], OutSpecs)
txpWorkers =
    merge $ []
  where
    merge = mconcatPair . map (first pure)
