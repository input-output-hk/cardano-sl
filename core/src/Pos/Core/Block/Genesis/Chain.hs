{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Definitions of the genesis blockchain ('Blockchain' class and related).

module Pos.Core.Block.Genesis.Chain
       ( BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable

import           Pos.Core.Block.Blockchain (Blockchain (..))
import           Pos.Core.Block.Genesis.Types (GenesisBlock, GenesisBlockchain,
                                               GenesisExtraBodyData, GenesisExtraHeaderData)
import           Pos.Core.Block.Union.Types (Block, BlockHeader)
import           Pos.Core.Common (ChainDifficulty, SlotLeaders)
import           Pos.Core.Slotting.Types (EpochIndex (..))
import           Pos.Crypto (Hash, hash)

instance Blockchain GenesisBlockchain where
    -- [CSL-199]: maybe we should use ADS.
    -- | Proof of GenesisBody is just a hash of slot leaders list.
    data BodyProof GenesisBlockchain = GenesisProof
        !(Hash SlotLeaders)
        deriving (Eq, Generic, Show)
    data ConsensusData GenesisBlockchain = GenesisConsensusData
        { -- | Index of the slot for which this genesis block is relevant.
          _gcdEpoch :: !EpochIndex
        , -- | Difficulty of the chain ending in this genesis block.
          _gcdDifficulty :: !ChainDifficulty
        } deriving (Generic, Show, Eq)
    type BBlockHeader GenesisBlockchain = BlockHeader
    type ExtraHeaderData GenesisBlockchain = GenesisExtraHeaderData

    -- | Body of genesis block consists of slot leaders for epoch
    -- associated with this block.
    data Body GenesisBlockchain = GenesisBody
        { _gbLeaders :: !SlotLeaders
        } deriving (Generic, Show, Eq)

    type ExtraBodyData GenesisBlockchain = GenesisExtraBodyData
    type BBlock GenesisBlockchain = Block

    mkBodyProof = GenesisProof . hash . _gbLeaders

instance Buildable (BodyProof GenesisBlockchain) where
    build (GenesisProof h) = Buildable.build h

instance NFData (BodyProof GenesisBlockchain)
instance NFData (ConsensusData GenesisBlockchain)
instance NFData (Body GenesisBlockchain)
instance NFData GenesisBlock
