{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Definitions of the genesis blockchain ('Blockchain' class and related).

module Pos.Block.Core.Genesis.Chain
       ( BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       ) where

import           Universum

import           Pos.Block.Core.Genesis.Types (GenesisBlock, GenesisBlockchain,
                                               GenesisExtraBodyData,
                                               GenesisExtraHeaderData)
import           Pos.Block.Core.Union.Types   (Block, BlockHeader)
import           Pos.Core                     (Blockchain (..), BlockchainHelpers (..),
                                               ChainDifficulty, EpochIndex (..),
                                               SlotLeaders)
import           Pos.Crypto                   (Hash, hash)
import           Pos.Ssc.Class.Types          (Ssc (..))

instance Blockchain (GenesisBlockchain ssc) where
    -- [CSL-199]: maybe we should use ADS.
    -- | Proof of GenesisBody is just a hash of slot leaders list.
    data BodyProof (GenesisBlockchain ssc) = GenesisProof
        !(Hash SlotLeaders)
        deriving (Eq, Generic, Show)
    data ConsensusData (GenesisBlockchain ssc) = GenesisConsensusData
        { -- | Index of the slot for which this genesis block is relevant.
          _gcdEpoch :: !EpochIndex
        , -- | Difficulty of the chain ending in this genesis block.
          _gcdDifficulty :: !ChainDifficulty
        } deriving (Generic, Show, Eq)
    type BBlockHeader (GenesisBlockchain ssc) = BlockHeader ssc
    type ExtraHeaderData (GenesisBlockchain ssc) = GenesisExtraHeaderData

    -- | Body of genesis block consists of slot leaders for epoch
    -- associated with this block.
    data Body (GenesisBlockchain ssc) = GenesisBody
        { _gbLeaders :: !SlotLeaders
        } deriving (Generic, Show, Eq)

    type ExtraBodyData (GenesisBlockchain ssc) = GenesisExtraBodyData
    type BBlock (GenesisBlockchain ssc) = Block ssc

    mkBodyProof = GenesisProof . hash . _gbLeaders

instance BlockchainHelpers (GenesisBlockchain ssc) where
    verifyBBlockHeader _ = pure ()
    verifyBBlock _ = pure ()

instance (Ssc ssc) => NFData (BodyProof (GenesisBlockchain ssc))
instance (Ssc ssc) => NFData (ConsensusData (GenesisBlockchain ssc))
instance (Ssc ssc) => NFData (Body (GenesisBlockchain ssc))
instance (Ssc ssc) => NFData (GenesisBlock ssc)
