{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Universum

import           Pos.Worker.Block (blkWorkers)
-- Message instances.
import           Pos.Context (NodeContext (..))
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Network.CLI (launchStaticConfigMonitoring)
import           Pos.Infra.Network.Types (NetworkConfig (..))
import           Pos.Infra.Slotting (logNewSlotWorker)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Worker.Delegation (dlgWorkers)
import           Pos.Worker.Ssc (sscWorkers)
import           Pos.Worker.Update (usWorkers)
import           Pos.WorkMode (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ext ctx m .
       (HasTxpConfiguration, WorkMode ctx m)
    => ProtocolMagic
    -> NodeResources ext
    -> [Diffusion m -> m ()]
allWorkers pm NodeResources {..} = mconcat
    [ sscWorkers pm
    , usWorkers
    , blkWorkers pm
    , dlgWorkers
    , [properSlottingWorker, staticConfigMonitoringWorker]
    ]
  where
    topology = ncTopology ncNetworkConfig
    NodeContext {..} = nrContext
    properSlottingWorker = const logNewSlotWorker
    staticConfigMonitoringWorker = const (launchStaticConfigMonitoring topology)
