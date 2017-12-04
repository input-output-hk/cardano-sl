-- | LRC genesis data.

module Pos.Lrc.Genesis
    ( genesisLeaders
    ) where

import           Pos.Core (GenesisData (..), HasConfiguration, SlotLeaders, genesisData)
import           Pos.Lrc.FtsPure (followTheSatoshiUtxo)
import           Pos.Txp.GenesisUtxo (genesisUtxo)
import           Pos.Txp.Toil (GenesisUtxo (..))

-- | Compute leaders of the 0-th epoch from stake distribution.
genesisLeaders :: HasConfiguration => SlotLeaders
genesisLeaders = followTheSatoshiUtxo (gdFtsSeed genesisData) utxo
  where
    GenesisUtxo utxo = genesisUtxo
