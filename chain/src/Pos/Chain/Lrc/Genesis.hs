-- | Computation of LRC genesis data.

module Pos.Chain.Lrc.Genesis
    ( genesisLeaders
    ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBootStakeholders, configEpochSlots, configFtsSeed)
import           Pos.Chain.Lrc.Fts (followTheSatoshi)
import           Pos.Chain.Txp (Utxo, genesisUtxo, utxoToStakes)
import           Pos.Core (SlotLeaders)


-- | Compute leaders of the 0-th epoch from initial shared seed and stake distribution.
genesisLeaders :: Genesis.Config -> SlotLeaders
genesisLeaders genesisConfig = followTheSatoshiUtxo
    genesisConfig
    (genesisUtxo $ configGenesisData genesisConfig)

-- This should not be exported unless it is *needed* elsewhere
-- NB: here we rely on the ordering produced by HM.toList; if it changes,
-- `genesisLeaders` might start producing different results. Be careful with
-- this code!
--
-- See https://github.com/input-output-hk/cardano-sl/issues/3425
followTheSatoshiUtxo :: Genesis.Config -> Utxo -> SlotLeaders
followTheSatoshiUtxo genesisConfig utxo =
    followTheSatoshi
            (configEpochSlots genesisConfig)
            (configFtsSeed genesisConfig)
        $ HM.toList
        $ utxoToStakes (configBootStakeholders genesisConfig) utxo
