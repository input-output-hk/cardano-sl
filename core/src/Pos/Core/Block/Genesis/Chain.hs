{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Definitions of the genesis blockchain ('Blockchain' class and related).

module Pos.Core.Block.Genesis.Chain
       (
       ) where

import           Universum

import           Pos.Core.Block.Blockchain (Blockchain (..))
import           Pos.Core.Block.Genesis.Types (GenesisBlockchain, GenesisBody (..),
                                               GenesisConsensusData, GenesisExtraBodyData,
                                               GenesisExtraHeaderData, GenesisProof (..))
import           Pos.Core.Block.Union.Types (Block, BlockHeader)
import           Pos.Crypto (hash)

instance Blockchain GenesisBlockchain where
    type BodyProof GenesisBlockchain = GenesisProof
    type ConsensusData GenesisBlockchain = GenesisConsensusData
    type BBlockHeader GenesisBlockchain = BlockHeader
    type ExtraHeaderData GenesisBlockchain = GenesisExtraHeaderData

    type Body GenesisBlockchain = GenesisBody

    type ExtraBodyData GenesisBlockchain = GenesisExtraBodyData
    type BBlock GenesisBlockchain = Block

    mkBodyProof = GenesisProof . hash . _gbLeaders
