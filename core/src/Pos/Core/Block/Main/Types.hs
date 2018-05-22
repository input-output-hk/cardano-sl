-- | Types defining the main blockchain.

module Pos.Core.Block.Main.Types
       ( MainBlockchain
       , MainProof (..)
       , MainConsensusData (..)
       , MainExtraHeaderData (..)
       , MainBlockHeader
       , MainBody (..)
       , MainExtraBodyData (..)
       , MainBlock

       , BlockHeaderAttributes
       , BlockBodyAttributes
       , BlockSignature (..)
       , MainToSign (..)
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Fmt (genericF)
import           Formatting (bprint, build, builder, (%))

import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Core.Common (ChainDifficulty, HeaderHash)
import           Pos.Core.Delegation (DlgPayload, ProxySigHeavy, ProxySigLight)
import           Pos.Core.Slotting.Types (SlotId (..))
import           Pos.Core.Ssc (SscPayload, SscProof)
import           Pos.Core.Txp (TxPayload, TxProof)
import           Pos.Core.Update (UpdatePayload, UpdateProof)
import           Pos.Core.Update.Types (BlockVersion, SoftwareVersion)
import           Pos.Crypto (Hash, PublicKey, Signature)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with actual payload (transactions, SSC, update system, etc.).
data MainBlockchain

-- | Proof of everything contained in the payload.
data MainProof = MainProof
    { mpTxProof       :: !TxProof
    , mpMpcProof      :: !SscProof
    , mpProxySKsProof :: !(Hash DlgPayload)
    , mpUpdateProof   :: !UpdateProof
    } deriving (Eq, Show, Generic)

instance NFData MainProof

instance Buildable MainProof where
    build = genericF

-- | Signature of the block. Can be either regular signature from the
-- issuer or delegated signature having a constraint on epoch indices
-- (it means the signature is valid only if block's slot id has epoch
-- inside the constrained interval).
data BlockSignature
    = BlockSignature (Signature MainToSign)
    | BlockPSignatureLight (ProxySigLight MainToSign)
    | BlockPSignatureHeavy (ProxySigHeavy MainToSign)
    deriving (Show, Eq, Generic)

instance NFData MainProof => NFData BlockSignature

instance Buildable BlockSignature where
    build (BlockSignature s)       = bprint ("BlockSignature: "%build) s
    build (BlockPSignatureLight s) = bprint ("BlockPSignatureLight: "%build) s
    build (BlockPSignatureHeavy s) = bprint ("BlockPSignatureHeavy: "%build) s

-- | Data to be signed in main block.
data MainToSign
    = MainToSign
    { _msHeaderHash  :: !HeaderHash  -- ^ Hash of previous header
                                     --    in the chain
    , _msBodyProof   :: !MainProof
    , _msSlot        :: !SlotId
    , _msChainDiff   :: !ChainDifficulty
    , _msExtraHeader :: !MainExtraHeaderData
    } deriving Generic

deriving instance Show MainToSign
deriving instance Eq MainToSign

data MainConsensusData = MainConsensusData
    { -- | Id of the slot for which this block was generated.
      _mcdSlot       :: !SlotId
    , -- | Public key of the slot leader. It's essential to have it here,
      -- because FTS gives us only hash of public key (aka 'StakeholderId').
      _mcdLeaderKey  :: !PublicKey
    , -- | Difficulty of chain ending in this block.
      _mcdDifficulty :: !ChainDifficulty
    , -- | Signature given by slot leader.
      _mcdSignature  :: !BlockSignature
    } deriving (Generic, Show, Eq)

instance NFData MainConsensusData

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
    , -- | Extra body data Hash
      _mehEBDataProof     :: !(Hash MainExtraBodyData)
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
            | areAttributesKnown _mehAttributes = mempty
            | otherwise = bprint ("    attributes: "%build%"\n") _mehAttributes

-- | Header of generic main block.
type MainBlockHeader = GenericBlockHeader MainBlockchain

-- | In our cryptocurrency, body consists of payloads of all block
-- components.
data MainBody = MainBody
    { -- | Txp payload.
      _mbTxPayload     :: !TxPayload
    , -- | Ssc payload.
      _mbSscPayload    :: !SscPayload
    , -- | Heavyweight delegation payload (no-ttl certificates).
      _mbDlgPayload    :: !DlgPayload
      -- | Additional update information for the update system.
    , _mbUpdatePayload :: !UpdatePayload
    } deriving (Eq, Show, Generic, Typeable)

instance NFData MainBody

-- | Represents main block body attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending block with new
-- fields via softfork.
type BlockBodyAttributes = Attributes ()

-- | Represents main block extra data
newtype MainExtraBodyData = MainExtraBodyData
    { _mebAttributes  :: BlockBodyAttributes
    } deriving (Eq, Show, Generic, NFData)

instance Buildable MainExtraBodyData where
    build (MainExtraBodyData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock = GenericBlock MainBlockchain
