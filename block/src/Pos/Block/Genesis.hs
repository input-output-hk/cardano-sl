-- | Function that creates the genesis block.

module Pos.Block.Genesis
       ( genesisBlock0
       ) where

import           Universum

import           Pos.Block.Base (mkGenesisBlock)
import           Pos.Core (HasConfiguration)
import           Pos.Core.Block (GenesisBlock)
import           Pos.Lrc.Genesis (genesisLeaders)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisBlock0 :: HasConfiguration => GenesisBlock
genesisBlock0 = mkGenesisBlock Nothing 0 genesisLeaders
