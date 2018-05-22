-- | Types defining the genesis blockchain.

module Pos.Core.Block.Genesis.Types
       ( GenesisBlockchain
       , GenesisProof (..)
       , GenesisConsensusData (..)
       , GenesisBlockHeader
       , GenesisBody (..)
       , GenesisBlock
       , GenesisExtraBodyData (..)
       , GenesisBodyAttributes
       , GenesisExtraHeaderData (..)
       , GenesisHeaderAttributes
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Core.Common (ChainDifficulty, SlotLeaders)
import           Pos.Core.Slotting.Types (EpochIndex (..))
import           Pos.Crypto (Hash)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

-- | Represents blockchain consisting of genesis blocks.  Genesis
-- block doesn't have any special payload and is not strictly
-- necessary. However, it is good idea to store list of leaders
-- explicitly, because calculating it may be expensive operation. For
-- example, it is useful for SPV-clients.
data GenesisBlockchain

-- [CSL-199]: maybe we should use ADS.
-- | Proof of GenesisBody is just a hash of slot leaders list.
data GenesisProof = GenesisProof
    !(Hash SlotLeaders)
    deriving (Eq, Generic, Show)

instance NFData GenesisProof

instance Buildable GenesisProof where
    build (GenesisProof h) = Buildable.build h

data GenesisConsensusData = GenesisConsensusData
    { -- | Index of the slot for which this genesis block is relevant.
      _gcdEpoch      :: !EpochIndex
    , -- | Difficulty of the chain ending in this genesis block.
      _gcdDifficulty :: !ChainDifficulty
    } deriving (Generic, Show, Eq)

instance NFData GenesisConsensusData

-- | Represents genesis block header attributes.
type GenesisHeaderAttributes = Attributes ()

-- | Represents genesis block header extra data
data GenesisExtraHeaderData = GenesisExtraHeaderData
    { -- | Header attributes
      _gehAttributes      :: !GenesisHeaderAttributes
    } deriving (Eq, Show, Generic)

instance NFData GenesisExtraHeaderData

instance Buildable GenesisExtraHeaderData where
    build (GenesisExtraHeaderData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

-- | Header of Genesis block.
type GenesisBlockHeader = GenericBlockHeader GenesisBlockchain

-- | Body of genesis block consists of slot leaders for epoch
-- associated with this block.
data GenesisBody = GenesisBody
    { _gbLeaders :: !SlotLeaders
    } deriving (Generic, Show, Eq)

instance NFData GenesisBody

-- | Represents genesis block header attributes.
type GenesisBodyAttributes = Attributes ()

-- | Represents genesis block header extra data
data GenesisExtraBodyData = GenesisExtraBodyData
    { -- | Header attributes
      _gebAttributes      :: !GenesisBodyAttributes
    } deriving (Eq, Show, Generic)

instance NFData GenesisExtraBodyData

instance Buildable GenesisExtraBodyData where
    build (GenesisExtraBodyData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

-- | Genesis block parametrized by 'GenesisBlockchain'.
type GenesisBlock = GenericBlock GenesisBlockchain
