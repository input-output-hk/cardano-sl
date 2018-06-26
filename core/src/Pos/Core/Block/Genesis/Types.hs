-- | Types defining the genesis blockchain.

module Pos.Core.Block.Genesis.Types
       ( GenesisProof (..)
       , GenesisConsensusData (..)
       , GenesisBody (..)
       , GenesisExtraBodyData (..)
       , GenesisBodyAttributes
       , GenesisExtraHeaderData (..)
       , GenesisHeaderAttributes
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                                   deriveSimpleBi, encodeListLen,
                                   enforceSize)
import           Pos.Core.Common (ChainDifficulty, SlotLeaders)
import           Pos.Core.Slotting (EpochIndex (..))
import           Pos.Crypto (Hash)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

-- [CSL-199]: maybe we should use ADS.
-- | Proof of GenesisBody is just a hash of slot leaders list.
data GenesisProof = GenesisProof
    !(Hash SlotLeaders)
    deriving (Eq, Generic, Show)

instance NFData GenesisProof

instance Buildable GenesisProof where
    build (GenesisProof h) = Buildable.build h

instance Bi GenesisProof where
    encode (GenesisProof h) = encode h
    decode = GenesisProof <$> decode

data GenesisConsensusData = GenesisConsensusData
    { -- | Index of the slot for which this genesis block is relevant.
      _gcdEpoch      :: !EpochIndex
    , -- | Difficulty of the chain ending in this genesis block.
      _gcdDifficulty :: !ChainDifficulty
    } deriving (Generic, Show, Eq)

instance NFData GenesisConsensusData

instance Bi GenesisConsensusData where
    encode bc =  encodeListLen 2
              <> encode (_gcdEpoch bc)
              <> encode (_gcdDifficulty bc)
    decode = do
      enforceSize "ConsensusData GenesisBlockchain" 2
      GenesisConsensusData <$> decode <*> decode

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

-- | Body of genesis block consists of slot leaders for epoch
-- associated with this block.
data GenesisBody = GenesisBody
    { _gbLeaders :: !SlotLeaders
    } deriving (Generic, Show, Eq)

instance Bi GenesisBody where
    encode = encode . _gbLeaders
    decode = GenesisBody <$> decode

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

deriveSimpleBi ''GenesisExtraHeaderData [
    Cons 'GenesisExtraHeaderData [
        Field [| _gehAttributes :: GenesisHeaderAttributes |]
    ]]

deriveSimpleBi ''GenesisExtraBodyData [
    Cons 'GenesisExtraBodyData [
        Field [| _gebAttributes :: GenesisBodyAttributes |]
    ]]
