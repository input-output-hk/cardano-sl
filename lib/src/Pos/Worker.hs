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
import           Pos.Core (ProtocolConstants, pcBlkSecurityParam, pcEpochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Delegation.Worker (dlgWorkers)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Network.CLI (launchStaticConfigMonitoring)
import           Pos.Infra.Network.Types (NetworkConfig (..))
import           Pos.Infra.Slotting (logNewSlotWorker)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Ssc.Worker (sscWorkers)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Update.Worker (usWorkers)
import           Pos.WorkMode (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ext ctx m .
       (HasTxpConfiguration, WorkMode ctx m)
    => ProtocolMagic
    -> ProtocolConstants
    -> NodeResources ext
    -> [Diffusion m -> m ()]
allWorkers pm pc NodeResources {..} = mconcat
    [ sscWorkers pm pc
    , usWorkers (pcBlkSecurityParam pc)
    , blkWorkers pm pc
    , dlgWorkers
    , [properSlottingWorker, staticConfigMonitoringWorker]
    ]
  where
    topology = ncTopology ncNetworkConfig
    NodeContext {..} = nrContext
    properSlottingWorker = const $ logNewSlotWorker $ pcEpochSlots pc
    staticConfigMonitoringWorker = const (launchStaticConfigMonitoring topology)
