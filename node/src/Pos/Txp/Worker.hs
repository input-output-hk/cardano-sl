module Pos.Txp.Worker
       ( txpWorkers
       ) where

import           Universum

import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Util            (mconcatPair)

-- | All workers specific to transaction processing.
txpWorkers
    :: ([WorkerSpec m], OutSpecs)
txpWorkers =
    merge $ []
  where
    merge = mconcatPair . map (first pure)
