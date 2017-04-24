{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Block.Types
       ( MainBlockchain
       , MainBlockHeader
       , MainExtraBodyData (..)
       , MainExtraHeaderData (..)
       , BlockHeaderAttributes
       , BlockBodyAttributes
       , BiSsc
       , BlockSignature (..)
       , MainToSign (..)
       , MainBlock

       , GenesisBlockchain
       , GenesisBlockHeader
       , GenesisBlock
       , GenesisExtraBodyData (..)
       , GenesisBodyAttributes
       , GenesisExtraHeaderData (..)
       , GenesisHeaderAttributes

       , BlockHeader
       , Block
       , BiHeader

       , mehBlockVersion
       , mehSoftwareVersion
       , mehAttributes
       , mebAttributes

       , gehAttributes
       , gebAttributes
       ) where

import           Control.Lens        (makeLenses)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, builder, (%))
import           Universum

import           Pos.Binary.Class    (Bi)
import           Pos.Binary.Core     ()
import           Pos.Binary.Crypto   ()
import           Pos.Core.Block      (Blockchain (..), GenericBlock (..),
                                      GenericBlockHeader (..))
import           Pos.Core.Types      (BlockVersion, ChainDifficulty, HeaderHash,
                                      ProxySigHeavy, ProxySigLight, SlotId (..),
                                      SoftwareVersion)
import           Pos.Crypto          (Signature)
import           Pos.Data.Attributes (Attributes (attrRemain))
import           Pos.Ssc.Class.Types (Ssc (..))

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with transactions and MPC messages.
data MainBlockchain ssc

-- | Data to be signed in main block.
data MainToSign ssc
    = MainToSign
    { _msHeaderHash  :: !HeaderHash
    , _msBodyProof   :: !(BodyProof (MainBlockchain ssc))
    , _msSlot        :: !SlotId
    , _msChainDiff   :: !ChainDifficulty
    , _msExtraHeader :: !MainExtraHeaderData
    }

-- | Signature of the block. Can be either regular signature from the
-- issuer or delegated signature having a constraint on epoch indices
-- (it means the signature is valid only if block's slot id has epoch
-- inside the constrained interval).
data BlockSignature ssc
    = BlockSignature (Signature (MainToSign ssc))
    | BlockPSignatureEpoch (ProxySigLight (MainToSign ssc))
    | BlockPSignatureSimple (ProxySigHeavy (MainToSign ssc))
    deriving (Show, Eq, Generic)

instance NFData (BodyProof (MainBlockchain ssc)) => NFData (BlockSignature ssc)

instance Buildable (BlockSignature ssc) where
    build (BlockSignature s)        = bprint ("BlockSignature: "%build) s
    build (BlockPSignatureEpoch s)  = bprint ("BlockPSignatureEpoch: "%build) s
    build (BlockPSignatureSimple s) = bprint ("BlockPSignatureSimple: "%build) s

-- | Represents main block body attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending block with new
-- fields via softfork.
type BlockBodyAttributes = Attributes ()

-- | Represents main block header attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending header with new
-- fields via softfork.
type BlockHeaderAttributes = Attributes ()

-- | Represents main block header extra data
data MainExtraHeaderData = MainExtraHeaderData
    { -- | Version of block.
      _mehBlockVersion    :: !BlockVersion
    , -- | Software version.
      _mehSoftwareVersion :: !SoftwareVersion
    , -- | Header attributes
      _mehAttributes      :: !BlockHeaderAttributes
    } deriving (Eq, Show, Generic)

instance NFData MainExtraHeaderData

instance Buildable MainExtraHeaderData where
    build MainExtraHeaderData {..} =
      bprint ( "    block: v"%build%"\n"
             % "    software: "%build%"\n"
             % builder
             )
            _mehBlockVersion
            _mehSoftwareVersion
            formattedExtra
      where
        formattedExtra
            | null (attrRemain _mehAttributes) = mempty
            | otherwise = bprint ("    attributes: "%build%"\n") _mehAttributes

-- | Represents main block extra data
newtype MainExtraBodyData = MainExtraBodyData
    { _mebAttributes  :: BlockBodyAttributes
    } deriving (Eq, Show, Generic, NFData)

instance Buildable MainExtraBodyData where
    build (MainExtraBodyData attrs)
        | null (attrRemain attrs) = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

-- | Header of generic main block.
type MainBlockHeader ssc = GenericBlockHeader (MainBlockchain ssc)

-- | Ssc w/ buildable blockchain
type BiSsc ssc =
    ( Ssc ssc
    , Bi (GenericBlockHeader (GenesisBlockchain ssc))
    , Bi (GenericBlockHeader (MainBlockchain ssc)))

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock ssc = GenericBlock (MainBlockchain ssc)

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Either header of ordinary main block or genesis block.
type BlockHeader ssc = Either (GenesisBlockHeader ssc) (MainBlockHeader ssc)

type BiHeader ssc = Bi (BlockHeader ssc)

-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------

makeLenses ''MainExtraHeaderData
makeLenses ''MainExtraBodyData

makeLenses ''GenesisExtraHeaderData
makeLenses ''GenesisExtraBodyData
