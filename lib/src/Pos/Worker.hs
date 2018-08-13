{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Universum

import           Pos.Worker.Block (blkWorkers)
-- Message instances.
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Context (NodeContext (..))
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Network.CLI (launchStaticConfigMonitoring)
import           Pos.Infra.Network.Types (NetworkConfig (..))
import           Pos.Infra.Slotting (logNewSlotWorker)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Util.Trace.Named (TraceNamed, natTrace)
import           Pos.Worker.Delegation (dlgWorkers)
import           Pos.Worker.Ssc (sscWorkers)
import           Pos.Worker.Update (usWorkers)
import           Pos.WorkMode (WorkMode)


-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ext ctx m . WorkMode ctx m
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> NodeResources ext
    -> [Diffusion m -> m ()]
allWorkers logTrace0 pm txpConfig NodeResources {..} = mconcat
    [ sscWorkers logTrace pm
    , usWorkers logTrace
    , blkWorkers logTrace0 pm txpConfig
    , dlgWorkers logTrace
    , [properSlottingWorker, staticConfigMonitoringWorker]
    ]
  where
    logTrace = natTrace liftIO logTrace0
    topology = ncTopology ncNetworkConfig
    NodeContext {..} = nrContext
    properSlottingWorker = const $ logNewSlotWorker logTrace
    staticConfigMonitoringWorker = const (launchStaticConfigMonitoring topology)
