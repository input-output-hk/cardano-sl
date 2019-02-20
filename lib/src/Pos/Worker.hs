{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Universum

import           Pos.Worker.Block (blkWorkers)
-- Message instances.
import           Pos.Chain.Genesis as Genesis (Config, configEpochSlots)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Context (NodeContext (..))
import           Pos.Core (StakeholderId)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Network.CLI (launchStaticConfigMonitoring)
import           Pos.Infra.Network.Types (NetworkConfig (..))
import           Pos.Infra.Slotting (logNewSlotWorker)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Worker.Delegation (dlgWorkers)
import           Pos.Worker.Ssc (sscWorkers)
import           Pos.Worker.Update (usWorkers)
import           Pos.WorkMode (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ext ctx m . WorkMode ctx m
    => StakeholderId
    -> Genesis.Config
    -> TxpConfiguration
    -> NodeResources ext
    -> [ (Text, Diffusion m -> m ()) ]
allWorkers sid genesisConfig txpConfig NodeResources {..} = mconcat
    [ sscWorkers sid genesisConfig
    , usWorkers genesisConfig
    , blkWorkers genesisConfig txpConfig
    , dlgWorkers
    , [ ("proper slotting", properSlottingWorker), ("static config", staticConfigMonitoringWorker) ]
    ]
  where
    topology = ncTopology ncNetworkConfig
    NodeContext {..} = nrContext
    properSlottingWorker =
        const $ logNewSlotWorker $ configEpochSlots genesisConfig
    staticConfigMonitoringWorker = const (launchStaticConfigMonitoring topology)
