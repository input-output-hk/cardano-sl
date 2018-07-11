-- | Computation of LRC genesis data.

module Pos.Lrc.Genesis
    ( genesisLeaders
    ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Core (GenesisData (..), GenesisData, SharedSeed (..),
                     SlotCount, SlotLeaders)
import           Pos.Lrc.Fts (followTheSatoshi)
import           Pos.Txp.GenesisUtxo (genesisUtxo)
import           Pos.Txp.Toil (GenesisUtxo (..), Utxo, utxoToStakes)


-- | Compute leaders of the 0-th epoch from initial shared seed and stake distribution.
genesisLeaders :: GenesisData -> SlotCount -> SlotLeaders
genesisLeaders gd epochSlots =
    followTheSatoshiUtxo gd epochSlots (gdFtsSeed gd) utxo
  where
    GenesisUtxo utxo = genesisUtxo gd

-- This should not be exported unless it is *needed* elsewhere
followTheSatoshiUtxo
    :: GenesisData
    -> SlotCount
    -> SharedSeed
    -> Utxo
    -> SlotLeaders
followTheSatoshiUtxo gd epochSlots seed utxo =
    followTheSatoshi epochSlots seed $ HM.toList $ utxoToStakes gd utxo
