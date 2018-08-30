-- | Computation of LRC genesis data.

module Pos.Chain.Lrc.Genesis
    ( genesisLeaders
    ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Chain.Lrc.Fts (followTheSatoshi)
import           Pos.Chain.Txp (Utxo, genesisUtxo, utxoToStakes)
import           Pos.Core as Core (Config (..), SlotLeaders,
                     configBootStakeholders, configEpochSlots, configFtsSeed)


-- | Compute leaders of the 0-th epoch from initial shared seed and stake distribution.
genesisLeaders :: Core.Config -> SlotLeaders
genesisLeaders coreConfig =
    followTheSatoshiUtxo coreConfig (genesisUtxo $ configGenesisData coreConfig)

-- This should not be exported unless it is *needed* elsewhere
followTheSatoshiUtxo :: Core.Config -> Utxo -> SlotLeaders
followTheSatoshiUtxo coreConfig utxo =
    -- NB: here we rely on the ordering produced by HM.toList; if it changes,
    -- `genesisLeaders` might start producing different results. Be careful with
    -- this code!
    --
    -- See https://github.com/input-output-hk/cardano-sl/issues/3425
    followTheSatoshi (configEpochSlots coreConfig) (configFtsSeed coreConfig)
        $ HM.toList
        $ utxoToStakes (configBootStakeholders coreConfig) utxo
