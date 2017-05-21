{-# LANGUAGE TemplateHaskell #-}

-- | Types defining the genesis blockchain.

module Pos.Block.Core.Genesis.Types
       ( GenesisBlockchain
       , GenesisBlockHeader
       , GenesisBlock
       , GenesisExtraBodyData (..)
       , GenesisBodyAttributes
       , GenesisExtraHeaderData (..)
       , GenesisHeaderAttributes
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, (%))

import           Pos.Core.Block      (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Data.Attributes (Attributes (attrRemain))

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
        | null (attrRemain attrs) = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

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
        | null (attrRemain attrs) = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

-- | Represents blockchain consisting of genesis blocks.  Genesis
-- block doesn't have any special payload and is not strictly
-- necessary. However, it is good idea to store list of leaders
-- explicitly, because calculating it may be expensive operation. For
-- example, it is useful for SPV-clients.
data GenesisBlockchain ssc

-- | Header of Genesis block.
type GenesisBlockHeader ssc = GenericBlockHeader (GenesisBlockchain ssc)

-- | Genesis block parametrized by 'GenesisBlockchain'.
type GenesisBlock ssc = GenericBlock (GenesisBlockchain ssc)
