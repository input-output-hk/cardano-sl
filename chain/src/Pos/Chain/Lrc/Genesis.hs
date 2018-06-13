-- | Computation of LRC genesis data.

module Pos.Chain.Lrc.Genesis
    ( genesisLeaders
    ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Chain.Lrc.Fts (followTheSatoshi)
import           Pos.Chain.Txp (GenesisUtxo (..), Utxo, genesisUtxo,
                     utxoToStakes)
import           Pos.Core (HasGenesisData, SharedSeed (..), SlotCount,
                     SlotLeaders, genesisData)
import           Pos.Core.Genesis (GenesisData (..))


-- | Compute leaders of the 0-th epoch from initial shared seed and stake distribution.
genesisLeaders :: HasGenesisData => SlotCount -> SlotLeaders
genesisLeaders epochSlots =
    followTheSatoshiUtxo epochSlots (gdFtsSeed genesisData) utxo
  where
    GenesisUtxo utxo = genesisUtxo

-- This should not be exported unless it is *needed* elsewhere
followTheSatoshiUtxo
    :: HasGenesisData => SlotCount -> SharedSeed -> Utxo -> SlotLeaders
followTheSatoshiUtxo epochSlots seed utxo =
    -- NB: here we rely on the ordering produced by HM.toList; if it changes,
    -- `genesisLeaders` might start producing different results. Be careful with
    -- this code!
    --
    -- See https://github.com/input-output-hk/cardano-sl/issues/3425
    followTheSatoshi epochSlots seed $ HM.toList $ utxoToStakes utxo
