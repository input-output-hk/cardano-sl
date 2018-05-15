{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Universum

import           Pos.Block.Worker (blkWorkers)
-- Message instances.
import           Pos.Communication.Message ()
import           Pos.Context (NodeContext (..))
import           Pos.Delegation.Worker (dlgWorkers)
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Network.CLI (launchStaticConfigMonitoring)
import           Pos.Network.Types (NetworkConfig (..))
import           Pos.Slotting (logNewSlotWorker)
import           Pos.Ssc.Worker (sscWorkers)
import           Pos.Update.Worker (usWorkers)
import           Pos.WorkMode (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ext ctx m .
       WorkMode ctx m
    => NodeResources ext -> [Diffusion m -> m ()]
allWorkers NodeResources {..} = mconcat
    [ sscWorkers
    , usWorkers
    , blkWorkers
    , dlgWorkers
    , [properSlottingWorker, staticConfigMonitoringWorker]
    ]
  where
    topology = ncTopology ncNetworkConfig
    NodeContext {..} = nrContext
    properSlottingWorker = const logNewSlotWorker
    staticConfigMonitoringWorker = const (launchStaticConfigMonitoring topology)
