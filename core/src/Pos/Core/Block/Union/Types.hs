-- the Getter instances
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Union of blockchain types.

module Pos.Core.Block.Union.Types
       ( BlockHeader (BlockHeaderGenesis, BlockHeaderMain)
       , _BlockHeaderGenesis
       , _BlockHeaderMain
       , choosingBlockHeader
       , Block

       -- * GenesisBlockchain
       , GenesisBlockchain
       , GenesisBlockHeader
       , GenesisBlock

       -- * MainBlockchain
       , MainBlockchain
       , MainBlockHeader
       , MainBlock

       -- * MainConsensusData
       , MainConsensusData (..)
       , MainToSign (..)
       , BlockSignature (..)

       -- * HeaderHash related types and functions
       , HeaderHash
       , headerHashF
       , HasHeaderHash (..)
       , headerHashG
       , HasPrevBlock (..)

       , blockHeaderHash
       , blockHeaderProtocolMagic

       -- * IsHeader classes
       , IsHeader
       , IsGenesisHeader
       , IsMainHeader (..)

       , module Pos.Core.Block.Genesis.Types
       , module Pos.Core.Block.Main.Types
       ) where

import           Codec.CBOR.Decoding (decodeWordCanonical)
import           Codec.CBOR.Encoding (encodeWord)
import           Control.Lens (Getter, LensLike', choosing, makePrisms, to)
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, (%))
import           Universum

import           Pos.Binary.Class (Bi (..), decodeListLenCanonicalOf,
                                   encodeListLen, enforceSize)
import           Pos.Core.Block.Blockchain (Blockchain (..), GenericBlock (..),
                                            GenericBlockHeader (..), gbHeader, gbhPrevBlock)
import           Pos.Core.Block.Genesis.Types
import           Pos.Core.Block.Main.Types
import           Pos.Core.Common (ChainDifficulty, HasDifficulty (..))
import           Pos.Core.Delegation (ProxySigHeavy, ProxySigLight)
import           Pos.Core.Slotting (HasEpochIndex (..), HasEpochOrSlot (..), SlotId (..))
import           Pos.Core.Ssc (mkSscProof)
import           Pos.Core.Txp (mkTxProof)
import           Pos.Core.Update (HasBlockVersion (..), HasSoftwareVersion (..), mkUpdateProof)
import           Pos.Crypto (Hash, ProtocolMagic, PublicKey, Signature, hash, unsafeHash)
import           Pos.Util.Some (Some, applySome, liftLensSome)
import           Pos.Util.Util (cborError)

----------------------------------------------------------------------------
-- GenesisBlockchain
----------------------------------------------------------------------------

-- | Represents blockchain consisting of genesis blocks.  Genesis
-- block doesn't have any special payload and is not strictly
-- necessary. However, it is good idea to store list of leaders
-- explicitly, because calculating it may be expensive operation. For
-- example, it is useful for SPV-clients.
data GenesisBlockchain

-- | Header of Genesis block.
type GenesisBlockHeader = GenericBlockHeader GenesisBlockchain

-- | Genesis block parametrized by 'GenesisBlockchain'.
type GenesisBlock = GenericBlock GenesisBlockchain

instance Blockchain GenesisBlockchain where
    type BodyProof GenesisBlockchain = GenesisProof
    type ConsensusData GenesisBlockchain = GenesisConsensusData
    type BBlockHeader GenesisBlockchain = BlockHeader
    type BHeaderHash GenesisBlockchain = HeaderHash
    type ExtraHeaderData GenesisBlockchain = GenesisExtraHeaderData

    type Body GenesisBlockchain = GenesisBody

    type ExtraBodyData GenesisBlockchain = GenesisExtraBodyData
    type BBlock GenesisBlockchain = Block

    mkBodyProof = GenesisProof . hash . _gbLeaders

----------------------------------------------------------------------------
-- MainBlockchain
----------------------------------------------------------------------------

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with actual payload (transactions, SSC, update system, etc.).
data MainBlockchain

-- | Header of generic main block.
type MainBlockHeader = GenericBlockHeader MainBlockchain

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock = GenericBlock MainBlockchain

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

instance Bi BlockSignature where
    encode input = case input of
        BlockSignature sig       -> encodeListLen 2 <> encode (0 :: Word8) <> encode sig
        BlockPSignatureLight pxy -> encodeListLen 2 <> encode (1 :: Word8) <> encode pxy
        BlockPSignatureHeavy pxy -> encodeListLen 2 <> encode (2 :: Word8) <> encode pxy
    decode = do
        enforceSize "BlockSignature" 2
        tag <- decode @Word8
        case tag of
          0 -> BlockSignature <$> decode
          1 -> BlockPSignatureLight <$> decode
          2 -> BlockPSignatureHeavy <$> decode
          _ -> cborError $ "decode@BlockSignature: unknown tag: " <> show tag

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

instance Bi MainToSign where
    encode mts = encodeListLen 5
               <> encode (_msHeaderHash mts)
               <> encode (_msBodyProof mts)
               <> encode (_msSlot mts)
               <> encode (_msChainDiff mts)
               <> encode (_msExtraHeader mts)
    decode = do
        enforceSize "MainToSign" 5
        MainToSign <$> decode <*>
                          decode <*>
                          decode <*>
                          decode <*>
                          decode

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

instance Bi MainConsensusData where
    encode cd =  encodeListLen 4
              <> encode (_mcdSlot cd)
              <> encode (_mcdLeaderKey cd)
              <> encode (_mcdDifficulty cd)
              <> encode (_mcdSignature cd)
    decode = do
        enforceSize "ConsensusData MainBlockchain)" 4
        MainConsensusData <$> decode <*>
                                 decode <*>
                                 decode <*>
                                 decode

instance ( Bi BlockHeader
         , Bi MainProof) =>
         Blockchain MainBlockchain where

    type BodyProof MainBlockchain = MainProof

    type ConsensusData MainBlockchain = MainConsensusData

    type BBlockHeader MainBlockchain = BlockHeader
    type BHeaderHash MainBlockchain = HeaderHash
    type ExtraHeaderData MainBlockchain = MainExtraHeaderData

    type Body MainBlockchain = MainBody

    type ExtraBodyData MainBlockchain = MainExtraBodyData
    type BBlock MainBlockchain = Block

    mkBodyProof MainBody{..} =
        MainProof
        { mpTxProof = mkTxProof _mbTxPayload
        , mpMpcProof = mkSscProof _mbSscPayload
        , mpProxySKsProof = hash _mbDlgPayload
        , mpUpdateProof = mkUpdateProof _mbUpdatePayload
        }

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Either header of ordinary main block or genesis block.
data BlockHeader
    = BlockHeaderGenesis GenesisBlockHeader
    | BlockHeaderMain MainBlockHeader

deriving instance Generic BlockHeader
deriving instance (Eq GenesisBlockHeader, Eq MainBlockHeader) => Eq BlockHeader
deriving instance (Show GenesisBlockHeader, Show MainBlockHeader) => Show BlockHeader

instance
    ( NFData GenesisBlockHeader
    , NFData MainBlockHeader
    )
    => NFData BlockHeader where
    rnf (BlockHeaderGenesis header) = rnf header
    rnf (BlockHeaderMain header) = rnf header

choosingBlockHeader :: Functor f =>
       LensLike' f GenesisBlockHeader r
    -> LensLike' f MainBlockHeader r
    -> LensLike' f BlockHeader r
choosingBlockHeader onGenesis onMain f = \case
    BlockHeaderGenesis bh -> BlockHeaderGenesis <$> onGenesis f bh
    BlockHeaderMain bh -> BlockHeaderMain <$> onMain f bh

instance Bi BlockHeader where
   encode x = encodeListLen 2 <> encodeWord tag <> body
     where
       (tag, body) = case x of
         BlockHeaderGenesis bh -> (0, encode bh)
         BlockHeaderMain bh    -> (1, encode bh)

   decode = do
       decodeListLenCanonicalOf 2
       t <- decodeWordCanonical
       case t of
           0 -> BlockHeaderGenesis <$!> decode
           1 -> BlockHeaderMain <$!> decode
           _ -> cborError $ "decode@BlockHeader: unknown tag " <> pretty t

-- | Block.
type Block = Either GenesisBlock MainBlock

----------------------------------------------------------------------------
-- HeaderHash
----------------------------------------------------------------------------

-- | 'Hash' of block header.
type HeaderHash = Hash BlockHeader

-- | Specialized formatter for 'HeaderHash'.
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- HasHeaderHash
class HasHeaderHash a where
    headerHash :: a -> HeaderHash

instance HasHeaderHash HeaderHash where
    headerHash = identity

instance HasHeaderHash (Some HasHeaderHash) where
    headerHash = applySome headerHash

headerHashG :: HasHeaderHash a => Getter a HeaderHash
headerHashG = to headerHash

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
--
-- Perhaps, it shouldn't be here, but I decided not to create a module
-- for only this function.
blockHeaderHash :: Bi BlockHeader => BlockHeader -> HeaderHash
blockHeaderHash = unsafeHash

-- HasPrevBlock
-- | Class for something that has previous block (lens to 'Hash' for this block).
class HasPrevBlock s where
    prevBlockL :: Lens' s HeaderHash

instance HasPrevBlock (Some HasPrevBlock) where
    prevBlockL = liftLensSome prevBlockL

instance (HasPrevBlock s, HasPrevBlock s') =>
         HasPrevBlock (Either s s') where
    prevBlockL = choosing prevBlockL prevBlockL

-- Perhaps it is not the best instance.
instance {-# OVERLAPPABLE #-} HasPrevBlock s => HasPrevBlock (s, z) where
    prevBlockL = _1 . prevBlockL

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlockHeader b) where
    prevBlockL = gbhPrevBlock

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlock b) where
    prevBlockL = gbHeader . gbhPrevBlock

instance HasPrevBlock BlockHeader where
    prevBlockL = choosingBlockHeader prevBlockL prevBlockL


-- | The 'ProtocolMagic' in a 'BlockHeader'.
blockHeaderProtocolMagic :: BlockHeader -> ProtocolMagic
blockHeaderProtocolMagic (BlockHeaderGenesis gbh) = _gbhProtocolMagic gbh
blockHeaderProtocolMagic (BlockHeaderMain mbh)    = _gbhProtocolMagic mbh

makePrisms 'BlockHeaderGenesis

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

{- | A class that lets subpackages use some fields from headers without
depending on cardano-sl:

  * 'difficultyL'
  * 'epochIndexL'
  * 'epochOrSlotG'
  * 'prevBlockL'
  * 'headerHashG'
-}
class ( HasDifficulty header
      , HasEpochIndex header
      , HasEpochOrSlot header
      , HasPrevBlock header
      , HasHeaderHash header) =>
      IsHeader header

instance HasDifficulty (Some IsHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsHeader) where
    headerHash = applySome headerHash

instance IsHeader (Some IsHeader)

----------------------------------------------------------------------------
-- IsGenesisHeader
----------------------------------------------------------------------------

-- | A class for genesis headers.
class IsHeader header => IsGenesisHeader header

instance HasDifficulty (Some IsGenesisHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsGenesisHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsGenesisHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsGenesisHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsGenesisHeader) where
    headerHash = applySome headerHash

instance IsHeader        (Some IsGenesisHeader)
instance IsGenesisHeader (Some IsGenesisHeader)

----------------------------------------------------------------------------
-- IsMainHeader
----------------------------------------------------------------------------

{- | A class for main headers. In addition to 'IsHeader', provides:

  * 'headerSlotL'
  * 'headerLeaderKeyL'
  * 'blockVersionL'
  * 'softwareVersionL'
-}
class (IsHeader header
      ,HasBlockVersion header
      ,HasSoftwareVersion header) =>
      IsMainHeader header
  where
    -- | Id of the slot for which this block was generated.
    headerSlotL :: Lens' header SlotId
    -- | Public key of slot leader.
    headerLeaderKeyL :: Lens' header PublicKey

instance HasDifficulty (Some IsMainHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsMainHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsMainHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsMainHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsMainHeader) where
    headerHash = applySome headerHash

instance HasBlockVersion (Some IsMainHeader) where
    blockVersionL = liftLensSome blockVersionL

instance HasSoftwareVersion (Some IsMainHeader) where
    softwareVersionL = liftLensSome softwareVersionL

instance IsHeader     (Some IsMainHeader)
instance IsMainHeader (Some IsMainHeader) where
    headerSlotL = liftLensSome headerSlotL
    headerLeaderKeyL = liftLensSome headerLeaderKeyL
