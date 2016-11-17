{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Definitions of the most fundamental types.

module Pos.Types.Types
       (
         Coin (..)
       , coinF

       , EpochIndex (..)
       , LocalSlotIndex (..)
       , SlotId (..)
       , FlatSlotId
       , slotIdF

       , Address (..)
       , addressF

       , TxSig
       , TxId
       , TxIn (..)
       , TxOut (..)
       , Tx (..)
       , txF

       , Utxo
       , formatUtxo
       , utxoF

       , FtsSeed (..)
       , SharedSeed
       , SlotLeaders

       , Blockchain (..)
       , BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       , GenericBlockHeader (..)
       , GenericBlock (..)

       , MainBlockchain
       , MainBlockHeader
       , ChainDifficulty (..)
       , MainToSign
       , MainBlock

       , GenesisBlockchain
       , GenesisBlockHeader
       , GenesisBlock

       , BlockHeader
       , HeaderHash
       , Block
       , headerHashF

       -- * Lenses
       , HasDifficulty (..)
       , HasEpochIndex (..)
       , HasHeaderHash (..)
       , HasPrevBlock (..)

       , blockHeader
       , blockLeaderKey
       , blockLeaders
       , blockMpc
       , blockSignature
       , blockSlot
       , blockTxs
       , gbBody
       , gbBodyProof
       , gbExtra
       , gbHeader
       , gcdDifficulty
       , gcdEpoch
       , gbhExtra
       , gbhPrevBlock
       , gbhBodyProof
       , getBlockHeader
       , headerDifficulty
       , headerLeaderKey
       , headerSignature
       , headerSlot
       , mbMpc
       , mbTxs
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature

       -- TODO: move it from here to Block.hs
       , blockDifficulty
       , mkGenericBlock
       , mkGenericHeader
       , mkMainBlock
       , mkMainBody
       , mkMainHeader
       , mkGenesisHeader
       , mkGenesisBlock

       , VerifyBlockParams (..)
       , VerifyHeaderExtra (..)
       , verifyBlock
       , verifyBlocks
       , verifyGenericBlock
       -- , verifyGenericHeader
       , verifyHeader
       ) where

import           Control.Lens           (Getter, Lens', choosing, ix, makeLenses, to,
                                         view, (^.), (^?), _3)
import           Data.Binary            (Binary)
import           Data.Binary.Orphans    ()
import qualified Data.ByteString        as BS (pack, zipWith)
import qualified Data.ByteString.Char8  as BSC (pack)
import           Data.Data              (Data)
import           Data.Default           (Default (def))
import           Data.DeriveTH          (derive, makeNFData)
import           Data.Hashable          (Hashable)
import           Data.Ix                (Ix)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as M (toList)
import           Data.MessagePack       (MessagePack (..))
import           Data.SafeCopy          (SafeCopy (..), base, contain,
                                         deriveSafeCopySimple, safeGet, safePut)
import           Data.Tagged            (untag)
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, int, later, ords, sformat,
                                         stext, (%))
import           Serokell.AcidState     ()
import qualified Serokell.Util.Base16   as B16
import           Serokell.Util.Text     (listJson, mapBuilderJson, pairBuilder)
import           Serokell.Util.Verify   (VerificationRes (..), verifyGeneric)
import           Universum

import           Pos.Constants          (epochSlots, ftsSeedLength)
import           Pos.Crypto             (Hash, PublicKey, SecretKey, Signature, hash,
                                         hashHexF, shortHashF, sign, toPublic, unsafeHash,
                                         verify)
import           Pos.Merkle             (MerkleRoot, MerkleTree, mkMerkleTree, mtRoot,
                                         mtSize)
import           Pos.Ssc.Class.Types    (SscTypes (..))
import           Pos.Util               (Color (Magenta), colorize)

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Num, Enum, Integral, Show, Ord, Real, Eq, Bounded, Generic, Binary, Hashable, Data, NFData)

instance MessagePack Coin

instance Buildable Coin where
    build = bprint (int%" coin(s)")

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Generic, Binary, Hashable, Buildable)

instance MessagePack EpochIndex

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Binary, Hashable, Buildable)

instance MessagePack LocalSlotIndex

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic)

instance Binary SlotId
instance MessagePack SlotId

instance Buildable SlotId where
    build SlotId {..} =
        bprint (ords%" slot of "%ords%" epoch") siSlot siEpoch

slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- | Address is where you can send coins.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show, Eq, Generic, Buildable, Ord, Binary, Hashable, NFData)

instance MessagePack Address

addressF :: Format r (Address -> r)
addressF = build

----------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------

type TxId = Hash Tx

type TxSig = Signature (TxId, Word32, [TxOut])

-- | Transaction input.
data TxIn = TxIn
    { txInHash  :: !TxId    -- ^ Which transaction's output is used
    , txInIndex :: !Word32  -- ^ Index of the output in transaction's
                            -- outputs
    , txInSig   :: !TxSig   -- ^ Signature given by public key
                            -- corresponding to address referenced by
                            -- this input.
    } deriving (Eq, Ord, Show, Generic)

instance Binary TxIn
instance Hashable TxIn
instance MessagePack TxIn

instance Buildable TxIn where
    build TxIn {..} = bprint ("TxIn "%shortHashF%" #"%int) txInHash txInIndex

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Show, Generic)

instance Binary TxOut
instance Hashable TxOut
instance MessagePack TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

-- | Transaction.
data Tx = Tx
    { txInputs  :: ![TxIn]   -- ^ Inputs of transaction.
    , txOutputs :: ![TxOut]  -- ^ Outputs of transaction.
    } deriving (Eq, Ord, Show, Generic)

instance Binary Tx
instance Hashable Tx
instance MessagePack Tx

instance Buildable Tx where
    build Tx {..} =
        bprint
            ("Transaction with inputs "%listJson%", outputs: "%listJson)
            txInputs txOutputs

txF :: Format r (Tx -> r)
txF = build

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map (TxId, Word32) TxOut

formatUtxo :: Utxo -> Builder
formatUtxo = mapBuilderJson . map (first pairBuilder) . M.toList

utxoF :: Format r (Utxo -> r)
utxoF = later formatUtxo

----------------------------------------------------------------------------
-- SSC. It means shared seed computation, btw
----------------------------------------------------------------------------

-- | This is a shared seed used for follow-the-satoshi. This seed is
-- randomly generated by each party and eventually they agree on the
-- same value.
-- TODO: rename it into SharedSeed!
newtype FtsSeed = FtsSeed
    { getFtsSeed :: ByteString
    } deriving (Show, Eq, Ord, Generic, Binary, NFData)

instance MessagePack FtsSeed

instance Buildable FtsSeed where
    build = B16.formatBase16 . getFtsSeed

-- TODO: rename completely!
type SharedSeed = FtsSeed

instance Semigroup FtsSeed where
    (<>) (FtsSeed a) (FtsSeed b) =
        FtsSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules

instance Monoid FtsSeed where
    mempty = FtsSeed $ BSC.pack $ replicate ftsSeedLength '\NUL'
    mappend = (<>)
    mconcat = foldl' (<>) mempty

type SlotLeaders = NonEmpty PublicKey

----------------------------------------------------------------------------
-- GenericBlock
----------------------------------------------------------------------------

-- | Blockchain type class generalizes some functionality common for
-- different blockchains.
class Blockchain p where
    -- | Proof of data stored in the body. Ensures immutability.
    data BodyProof p :: *
    -- | Consensus data which can be used to check consensus properties.
    data ConsensusData p :: *
    -- | Whatever extra data.
    type ExtraHeaderData p :: *
    type ExtraHeaderData p = ()
    -- | Block header used in this blockchain.
    type BBlockHeader p :: *
    type BBlockHeader p = GenericBlockHeader p

    -- | Body contains payload and other heavy data.
    data Body p :: *
    -- | Whatever extra data.
    type ExtraBodyData p :: *
    type ExtraBodyData p = ()
    -- | Block used in this blockchain.
    type BBlock p :: *
    type BBlock p = GenericBlock p

    mkBodyProof :: Body p -> BodyProof p
    checkBodyProof :: Body p -> BodyProof p -> Bool
    default checkBodyProof :: Eq (BodyProof p) => Body p -> BodyProof p -> Bool
    checkBodyProof body proof = mkBodyProof body == proof

-- | Header of block contains some kind of summary. There are various
-- benefits which people get by separating header from other data.
data GenericBlockHeader b = GenericBlockHeader
    { -- | Pointer to the header of the previous block.
      _gbhPrevBlock :: !(Hash (BBlockHeader b))
    , -- | Proof of body.
      _gbhBodyProof :: !(BodyProof b)
    , -- | Consensus data to verify consensus algorithm.
      _gbhConsensus :: !(ConsensusData b)
    , -- | Any extra data.
      _gbhExtra     :: !(ExtraHeaderData b)
    } deriving (Generic)

deriving instance
         (Show (BodyProof b), Show (ConsensusData b),
          Show (ExtraHeaderData b)) =>
         Show (GenericBlockHeader b)

deriving instance
         (Eq (BodyProof b), Eq (ConsensusData b),
          Eq (ExtraHeaderData b)) =>
         Eq (GenericBlockHeader b)

instance ( Binary (BodyProof b)
         , Binary (ConsensusData b)
         , Binary (ExtraHeaderData b)
         ) =>
         Binary (GenericBlockHeader b)

-- | In general Block consists of header and body. It may contain
-- extra data as well.
data GenericBlock b = GenericBlock
    { _gbHeader :: !(GenericBlockHeader b)
    , _gbBody   :: !(Body b)
    , _gbExtra  :: !(ExtraBodyData b)
    } deriving (Generic)

deriving instance
         (Show (GenericBlockHeader b), Show (Body b),
          Show (ExtraBodyData b)) =>
         Show (GenericBlock b)

deriving instance
         (Eq (BodyProof b), Eq (ConsensusData b), Eq (ExtraHeaderData b),
          Eq (Body b), Eq (ExtraBodyData b)) =>
         Eq (GenericBlock b)

instance ( Binary (BodyProof b)
         , Binary (ConsensusData b)
         , Binary (ExtraHeaderData b)
         , Binary (Body b)
         , Binary (ExtraBodyData b)
         ) =>
         Binary (GenericBlock b)

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with transactions and MPC messages.
data MainBlockchain ssc

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Binary, Buildable)

instance MessagePack ChainDifficulty

type MainToSign ssc = (HeaderHash ssc, BodyProof (MainBlockchain ssc), SlotId, ChainDifficulty)

instance SscTypes ssc => Blockchain (MainBlockchain ssc) where
    -- | Proof of transactions list and MPC data.
    data BodyProof (MainBlockchain ssc) = MainProof
        { mpNumber   :: !Word32
        , mpRoot     :: !(MerkleRoot Tx)
        , mpMpcProof :: !(SscProof ssc)
        } deriving (Generic)
    data ConsensusData (MainBlockchain ssc) = MainConsensusData
        { -- | Id of the slot for which this block was generated.
        _mcdSlot       :: !SlotId
        , -- | Public key of slot leader. Maybe later we'll see it is redundant.
        _mcdLeaderKey  :: !PublicKey
        , -- | Difficulty of chain ending in this block.
        _mcdDifficulty :: !ChainDifficulty
        , -- | Signature given by slot leader.
        _mcdSignature  :: !(Signature (MainToSign ssc))
        } deriving (Generic, Show)
    type BBlockHeader (MainBlockchain ssc) = BlockHeader ssc

    -- | In our cryptocurrency, body consists of a list of transactions
    -- and MPC messages.
    data Body (MainBlockchain ssc) = MainBody
        { -- | Transactions are the main payload.
          -- TODO: currently we don't know for sure whether it should be
          -- MerkleTree or something list-like.
          _mbTxs         :: !(MerkleTree Tx)
        , -- | Data necessary for MPC.
          _mbMpc  :: !(SscPayload ssc)
        } deriving (Generic)
    type BBlock (MainBlockchain ssc) = Block ssc

    mkBodyProof MainBody {..} =
        MainProof
        { mpNumber = mtSize _mbTxs
        , mpRoot = mtRoot _mbTxs
        , mpMpcProof = untag @ssc mkSscProof _mbMpc
        }

deriving instance SscTypes ssc => Eq (BodyProof (MainBlockchain ssc))
deriving instance SscTypes ssc => Show (Body (MainBlockchain ssc))

instance SscTypes ssc => Binary (BodyProof (MainBlockchain ssc))
instance SscTypes ssc => Binary (ConsensusData (MainBlockchain ssc))
instance SscTypes ssc => Binary (Body (MainBlockchain ssc))

type MainBlockHeader ssc = GenericBlockHeader (MainBlockchain ssc)

instance SscTypes ssc => Buildable (MainBlockHeader ssc) where
    build gbh@GenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    leader: "%build%"\n"%
             "    difficulty: "%int%"\n"
            )
            headerHash
            _gbhPrevBlock
            _mcdSlot
            _mcdLeaderKey
            _mcdDifficulty
      where
        headerHash :: HeaderHash ssc
        headerHash = hash $ Right gbh
        MainConsensusData {..} = _gbhConsensus

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock ssc = GenericBlock (MainBlockchain ssc)

instance SscTypes ssc => Buildable (MainBlock ssc) where
    build GenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions: "%listJson%"\n"%
             build
            )
            (colorize Magenta "MainBlock")
            _gbHeader
            _mbTxs
            _mbMpc
      where
        MainBody {..} = _gbBody

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

-- | Represents blockchain consisting of genesis blocks.  Genesis
-- block doesn't have any special payload and is not strictly
-- necessary. However, it is good idea to store list of leaders
-- explicitly, because calculating it may be expensive operation. For
-- example, it is useful for SPV-clients.
data GenesisBlockchain ssc

type GenesisBlockHeader ssc = GenericBlockHeader (GenesisBlockchain ssc)

instance Blockchain (GenesisBlockchain ssc) where
    -- | Proof of GenesisBody is just a hash of slot leaders list.
    -- TODO: do we need a Merkle tree? This list probably won't be large.
    data BodyProof (GenesisBlockchain ssc) = GenesisProof
        !(Hash SlotLeaders)
        deriving (Eq, Generic, Show)
    data ConsensusData (GenesisBlockchain ssc) = GenesisConsensusData
        { -- | Index of the slot for which this genesis block is relevant.
          _gcdEpoch :: !EpochIndex
        , -- | Difficulty of the chain ending in this genesis block.
          _gcdDifficulty :: !ChainDifficulty
        } deriving (Generic, Show)
    type BBlockHeader (GenesisBlockchain ssc) = BlockHeader ssc

    -- | Body of genesis block consists of slot leaders for epoch
    -- associated with this block.
    data Body (GenesisBlockchain ssc) = GenesisBody
        { _gbLeaders :: !SlotLeaders
        } deriving (Show, Generic)
    type BBlock (GenesisBlockchain ssc) = Block ssc

    mkBodyProof = GenesisProof . hash . _gbLeaders

instance Binary (BodyProof (GenesisBlockchain ssc))
instance Binary (ConsensusData (GenesisBlockchain ssc))
instance Binary (Body (GenesisBlockchain ssc))

instance MessagePack (BodyProof (GenesisBlockchain ssc))
instance MessagePack (ConsensusData (GenesisBlockchain ssc))
instance MessagePack (Body (GenesisBlockchain ssc))

type GenesisBlock ssc = GenericBlock (GenesisBlockchain ssc)

instance SscTypes ssc => Buildable (GenesisBlock ssc) where
    build GenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             stext
            )
            (colorize Magenta "GenesisBlock")
            _gbHeader
            formatLeaders
      where
        GenesisBody {..} = _gbBody
        formatIfNotNull formatter l = if null l then mempty else sformat formatter l
        formatLeaders = formatIfNotNull
            ("  leaders: "%listJson%"\n") _gbLeaders

instance SscTypes ssc => Buildable (GenesisBlockHeader ssc) where
    build gbh@GenericBlockHeader {..} =
        bprint
            ("GenesisBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    epoch: "%build%"\n"%
             "    difficulty: "%int%"\n"
            )
            headerHash
            _gbhPrevBlock
            _gcdEpoch
            _gcdDifficulty
      where
        headerHash :: HeaderHash ssc
        headerHash = hash $ Left gbh
        GenesisConsensusData {..} = _gbhConsensus

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

type BlockHeader ssc = Either (GenesisBlockHeader ssc) (MainBlockHeader ssc)
type HeaderHash ssc = Hash (BlockHeader ssc)

headerHashF :: Format r (HeaderHash ssc -> r)
headerHashF = build

type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

----------------------------------------------------------------------------
-- Lenses. TODO: move to Block.hs and other modules or leave them here?
----------------------------------------------------------------------------

makeLenses ''GenericBlockHeader
makeLenses ''GenericBlock

-- TODO: 'makeLensesData' doesn't work with types with parameters. I don't
-- know how to design a 'makeLensesData' which would work with them (in fact,
-- I don't even know how an invocation of 'makeLensesData' would look like)

#define MAKE_LENS(l, field) l f s = (\y -> s {field = y}) <$> f (field s)

-- makeLensesData ''ConsensusData ''(MainBlockchain ssc)

mcdSlot :: Lens' (ConsensusData (MainBlockchain ssc)) SlotId
MAKE_LENS(mcdSlot, _mcdSlot)

mcdLeaderKey :: Lens' (ConsensusData (MainBlockchain ssc)) PublicKey
MAKE_LENS(mcdLeaderKey, _mcdLeaderKey)

mcdDifficulty :: Lens' (ConsensusData (MainBlockchain ssc)) ChainDifficulty
MAKE_LENS(mcdDifficulty, _mcdDifficulty)

mcdSignature :: Lens' (ConsensusData (MainBlockchain ssc)) (Signature (MainToSign ssc))
MAKE_LENS(mcdSignature, _mcdSignature)

-- makeLensesData ''ConsensusData ''(GenesisBlockchain ssc)

gcdEpoch :: Lens' (ConsensusData (GenesisBlockchain ssc)) EpochIndex
MAKE_LENS(gcdEpoch, _gcdEpoch)

gcdDifficulty :: Lens' (ConsensusData (GenesisBlockchain ssc)) ChainDifficulty
MAKE_LENS(gcdDifficulty, _gcdDifficulty)

-- makeLensesData ''Body ''(MainBlockchain ssc)

mbTxs :: Lens' (Body (MainBlockchain ssc)) (MerkleTree Tx)
MAKE_LENS(mbTxs, _mbTxs)

mbMpc :: Lens' (Body (MainBlockchain ssc)) (SscPayload ssc)
MAKE_LENS(mbMpc, _mbMpc)

-- makeLensesData ''Body ''(GenesisBlockchain ssc)

gbLeaders :: Lens' (Body (GenesisBlockchain ssc)) SlotLeaders
MAKE_LENS(gbLeaders, _gbLeaders)

gbBodyProof :: Lens' (GenericBlock b) (BodyProof b)
gbBodyProof = gbHeader . gbhBodyProof

headerSlot :: Lens' (MainBlockHeader ssc) SlotId
headerSlot = gbhConsensus . mcdSlot

headerLeaderKey :: Lens' (MainBlockHeader ssc) PublicKey
headerLeaderKey = gbhConsensus . mcdLeaderKey

headerSignature :: Lens' (MainBlockHeader ssc) (Signature (MainToSign ssc))
headerSignature = gbhConsensus . mcdSignature

class HasDifficulty a where
    difficultyL :: Lens' a ChainDifficulty

instance HasDifficulty (ConsensusData (MainBlockchain ssc)) where
    difficultyL = mcdDifficulty

instance HasDifficulty (ConsensusData (GenesisBlockchain ssc)) where
    difficultyL = gcdDifficulty

instance HasDifficulty (MainBlockHeader ssc) where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty (GenesisBlockHeader ssc) where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty (BlockHeader ssc) where
    difficultyL = choosing difficultyL difficultyL

instance HasDifficulty (MainBlock ssc) where
    difficultyL = gbHeader . difficultyL

instance HasDifficulty (GenesisBlock ssc) where
    difficultyL = gbHeader . difficultyL

instance HasDifficulty (Block ssc) where
    difficultyL = choosing difficultyL difficultyL

class HasPrevBlock s a | s -> a where
    prevBlockL :: Lens' s (Hash a)

instance (a ~ BBlockHeader b) =>
         HasPrevBlock (GenericBlockHeader b) a where
    prevBlockL = gbhPrevBlock

instance (a ~ BBlockHeader b) =>
         HasPrevBlock (GenericBlock b) a where
    prevBlockL = gbHeader . gbhPrevBlock

instance (HasPrevBlock s a, HasPrevBlock s' a) =>
         HasPrevBlock (Either s s') a where
    prevBlockL = choosing prevBlockL prevBlockL

class HasHeaderHash a ssc | a -> ssc where
    headerHash :: a -> HeaderHash ssc
    headerHashG :: Getter a (HeaderHash ssc)
    headerHashG = to headerHash

instance SscTypes ssc => HasHeaderHash (MainBlockHeader ssc) ssc where
    headerHash = hash . Right

instance SscTypes ssc => HasHeaderHash (GenesisBlockHeader ssc) ssc where
    headerHash = hash . Left

instance SscTypes ssc => HasHeaderHash (BlockHeader ssc) ssc where
    headerHash = hash

instance SscTypes ssc => HasHeaderHash (MainBlock ssc) ssc where
    headerHash = hash . Right . view gbHeader

instance SscTypes ssc => HasHeaderHash (GenesisBlock ssc) ssc where
    headerHash = hash . Left  . view gbHeader

instance SscTypes ssc => HasHeaderHash (Block ssc) ssc where
    headerHash = hash . getBlockHeader

class HasEpochIndex a where
    epochIndexL :: Lens' a EpochIndex

instance HasEpochIndex SlotId where
    epochIndexL f SlotId {..} = (\a -> SlotId {siEpoch = a, ..}) <$> f siEpoch

instance HasEpochIndex (MainBlock ssc) where
    epochIndexL = gbHeader . gbhConsensus . mcdSlot . epochIndexL

instance HasEpochIndex (GenesisBlock ssc) where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance (HasEpochIndex a, HasEpochIndex b) =>
         HasEpochIndex (Either a b) where
    epochIndexL = choosing epochIndexL epochIndexL

blockSlot :: Lens' (MainBlock ssc) SlotId
blockSlot = gbHeader . headerSlot

blockLeaderKey :: Lens' (MainBlock ssc) PublicKey
blockLeaderKey = gbHeader . headerLeaderKey

blockSignature :: Lens' (MainBlock ssc) (Signature (MainToSign ssc))
blockSignature = gbHeader . headerSignature

blockMpc :: Lens' (MainBlock ssc) (SscPayload ssc)
blockMpc = gbBody . mbMpc

blockTxs :: Lens' (MainBlock ssc) (MerkleTree Tx)
blockTxs = gbBody . mbTxs

blockLeaders :: Lens' (GenesisBlock ssc) SlotLeaders
blockLeaders = gbBody . gbLeaders

-- This gives a “redundant constraint” message warning which will be fixed in
-- lens-4.15 (not in LTS yet).
blockHeader :: Getter (Block ssc) (BlockHeader ssc)
blockHeader = to getBlockHeader

getBlockHeader :: Block ssc -> BlockHeader ssc
getBlockHeader = bimap (view gbHeader) (view gbHeader)

----------------------------------------------------------------------------
-- Block.hs. TODO: move it into Block.hs.
-- These functions are here because of GHC bug (trac 12127).
----------------------------------------------------------------------------

-- | Difficulty of the BlockHeader. 0 for genesis block, 1 for main block.
headerDifficulty :: BlockHeader ssc -> ChainDifficulty
headerDifficulty (Left _)  = 0
headerDifficulty (Right _) = 1

-- | Difficulty of the Block, which is determined from header.
blockDifficulty :: Block ssc -> ChainDifficulty
blockDifficulty = headerDifficulty . getBlockHeader

genesisHash :: Hash a
genesisHash = unsafeHash ("patak" :: Text)
{-# INLINE genesisHash #-}

mkGenericHeader
    :: forall b.
       (Binary (BBlockHeader b), Blockchain b)
    => Maybe (BBlockHeader b)
    -> Body b
    -> (Hash (BBlockHeader b) -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> GenericBlockHeader b
mkGenericHeader prevHeader body consensus extra =
    GenericBlockHeader
    { _gbhPrevBlock = h
    , _gbhBodyProof = proof
    , _gbhConsensus = consensus h proof
    , _gbhExtra = extra
    }
  where
    h :: Hash (BBlockHeader b)
    h = maybe genesisHash hash prevHeader
    proof = mkBodyProof body

mkGenericBlock
    :: forall b.
       (Binary (BBlockHeader b), Blockchain b)
    => Maybe (BBlockHeader b)
    -> Body b
    -> (Hash (BBlockHeader b) -> BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> ExtraBodyData b
    -> GenericBlock b
mkGenericBlock prevHeader body consensus extraH extraB =
    GenericBlock {_gbHeader = header, _gbBody = body, _gbExtra = extraB}
  where
    header = mkGenericHeader prevHeader body consensus extraH

mkMainHeader
    :: SscTypes ssc
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> Body (MainBlockchain ssc)
    -> MainBlockHeader ssc
mkMainHeader prevHeader slotId sk body =
    mkGenericHeader prevHeader body consensus ()
  where
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    signature prevHash proof = sign sk (prevHash, proof, slotId, difficulty)
    consensus prevHash proof =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = toPublic sk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature prevHash proof
        }

mkMainBlock
    :: SscTypes ssc
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> Body (MainBlockchain ssc)
    -> MainBlock ssc
mkMainBlock prevHeader slotId sk body =
    GenericBlock
    { _gbHeader = mkMainHeader prevHeader slotId sk body
    , _gbBody = body
    , _gbExtra = ()
    }

mkGenesisHeader
    :: SscTypes ssc
    => Maybe (BlockHeader ssc)
    -> EpochIndex
    -> Body (GenesisBlockchain ssc)
    -> GenesisBlockHeader ssc
mkGenesisHeader prevHeader epoch body =
    mkGenericHeader prevHeader body consensus ()
  where
    difficulty = maybe 0 (view difficultyL) prevHeader
    consensus _ _ =
        GenesisConsensusData {_gcdEpoch = epoch, _gcdDifficulty = difficulty}

mkGenesisBlock
    :: SscTypes ssc
    => Maybe (BlockHeader ssc)
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock ssc
mkGenesisBlock prevHeader epoch leaders =
    GenericBlock
    { _gbHeader = mkGenesisHeader prevHeader epoch body
    , _gbBody = body
    , _gbExtra = ()
    }
  where
    body = GenesisBody leaders

mkMainBody :: [Tx] -> SscPayload ssc -> Body (MainBlockchain ssc)
mkMainBody txs mpc = MainBody {_mbTxs = mkMerkleTree txs, _mbMpc = mpc}

verifyConsensusLocal :: SscTypes ssc => BlockHeader ssc -> VerificationRes
verifyConsensusLocal (Left _)       = mempty
verifyConsensusLocal (Right header) =
    verifyGeneric
        [ ( verify pk (_gbhPrevBlock, _gbhBodyProof, slotId, d) sig
          , "can't verify signature")
        , (siSlot slotId < epochSlots, "slot index is not less than epochSlots")
        ]
  where
    GenericBlockHeader {_gbhConsensus = consensus, ..} = header
    pk = consensus ^. mcdLeaderKey
    slotId = consensus ^. mcdSlot
    d = consensus ^. mcdDifficulty
    sig = consensus ^. mcdSignature

-- | Extra data which may be used by verifyHeader function to do more checks.
data VerifyHeaderExtra ssc = VerifyHeaderExtra
    { vhePrevHeader  :: !(Maybe (BlockHeader ssc))
    -- ^ Nothing means that block is unknown, not genesis.
    , vheNextHeader  :: !(Maybe (BlockHeader ssc))
    , vheCurrentSlot :: !(Maybe SlotId)
    , vheLeaders     :: !(Maybe SlotLeaders)
    }

-- | By default there is not extra data.
instance Default (VerifyHeaderExtra ssc) where
    def =
        VerifyHeaderExtra
        { vhePrevHeader = Nothing
        , vheNextHeader = Nothing
        , vheCurrentSlot = Nothing
        , vheLeaders = Nothing
        }

maybeEmpty :: Monoid m => (a -> m) -> Maybe a -> m
maybeEmpty = maybe mempty

-- | Check some predicates about BlockHeader. Number of checks depends
-- on extra data passed to this function. It tries to do as much as
-- possible.
verifyHeader
    :: SscTypes ssc
    => VerifyHeaderExtra ssc -> BlockHeader ssc -> VerificationRes
verifyHeader VerifyHeaderExtra {..} h =
    verifyConsensusLocal h <> verifyGeneric checks
  where
    checks =
        mconcat
            [ maybeEmpty relatedToPrevHeader vhePrevHeader
            , maybeEmpty relatedToNextHeader vheNextHeader
            , maybeEmpty relatedToCurrentSlot vheCurrentSlot
            , maybeEmpty relatedToLeaders vheLeaders
            ]
    checkHash expectedHash actualHash =
        ( expectedHash == actualHash
        , sformat
              ("inconsistent hash (expected " %build % ", found" %build % ")")
              expectedHash
              actualHash)
    checkDifficulty expectedDifficulty actualDifficulty =
        ( expectedDifficulty == actualDifficulty
        , sformat
              ("incorrect difficulty (expected " %int % ", found " %int % ")")
              expectedDifficulty
              actualDifficulty)
    relatedToPrevHeader prevHeader =
        [ checkDifficulty
              (prevHeader ^. difficultyL + headerDifficulty h)
              (h ^. difficultyL)
        , checkHash (hash prevHeader) (h ^. prevBlockL)
        ]
    relatedToNextHeader nextHeader =
        [ checkDifficulty
              (nextHeader ^. difficultyL - headerDifficulty nextHeader)
              (h ^. difficultyL)
        , checkHash (hash h) (nextHeader ^. prevBlockL)
        ]
    relatedToCurrentSlot curSlotId =
        [ ( either (const True) ((<= curSlotId) . view headerSlot) h
          , "block is from slot which hasn't happened yet")
        ]
    relatedToLeaders leaders =
        case h of
            Left _ -> []
            Right mainHeader ->
                [ ( (Just (mainHeader ^. headerLeaderKey) ==
                     leaders ^?
                     ix (fromIntegral $ siSlot $ mainHeader ^. headerSlot))
                  , "block's leader is different from expected one")
                ]

-- | Perform cheap checks of GenericBlock, which can be done using
-- only block itself. Checks which can be done using only header are
-- ignored here. It is assumed that they will be done separately.
verifyGenericBlock :: forall b . Blockchain b => GenericBlock b -> VerificationRes
verifyGenericBlock blk =
    verifyGeneric
        [ ( checkBodyProof (blk ^. gbBody) (blk ^. gbBodyProof)
          , "body proof doesn't prove body")
        ]

-- | Parameters of Block verification.
-- Note: to check that block references previous block and/or is referenced
-- by next block, use header verification (via vbpVerifyHeader).
data VerifyBlockParams ssc = VerifyBlockParams
    { vbpVerifyHeader  :: !(Maybe (VerifyHeaderExtra ssc))
    , vbpVerifyGeneric :: !Bool
    }

-- | By default nothing is checked.
instance Default (VerifyBlockParams ssc) where
    def =
        VerifyBlockParams
        { vbpVerifyHeader = Nothing
        , vbpVerifyGeneric = False
        }

-- | Check predicates defined by VerifyBlockParams.
verifyBlock :: SscTypes ssc => VerifyBlockParams ssc -> Block ssc -> VerificationRes
verifyBlock VerifyBlockParams {..} blk =
    mconcat
        [ verifyG
        , maybeEmpty (flip verifyHeader (getBlockHeader blk)) vbpVerifyHeader
        ]
  where
    verifyG =
        if vbpVerifyGeneric
            then either verifyGenericBlock verifyGenericBlock blk
            else mempty

-- | Verify sequence of blocks. It is assumed that the leftmost block
-- is the oldest one.
-- TODO: foldl' is used here which eliminates laziness benefits essential for
-- VerificationRes. Is it true? Can we do something with it?
-- Apart from returning Bool.
verifyBlocks
    :: forall ssc t. (SscTypes ssc, Foldable t)
    => Maybe SlotId -> t (Block ssc) -> VerificationRes
verifyBlocks curSlotId = (view _3) . foldl' step start
  where
    start :: (Maybe SlotLeaders, Maybe (BlockHeader ssc), VerificationRes)
    start = (Nothing, Nothing, mempty)
    step
        :: (Maybe SlotLeaders, Maybe (BlockHeader ssc), VerificationRes)
        -> Block ssc
        -> (Maybe SlotLeaders, Maybe (BlockHeader ssc), VerificationRes)
    step (leaders, prevHeader, res) blk =
        let newLeaders =
                case blk of
                    Left genesisBlock -> Just $ genesisBlock ^. blockLeaders
                    Right _           -> leaders
            vhe =
                VerifyHeaderExtra
                { vhePrevHeader = prevHeader
                , vheNextHeader = Nothing
                , vheLeaders = newLeaders
                , vheCurrentSlot = curSlotId
                }
            vbp =
                VerifyBlockParams
                {vbpVerifyHeader = Just vhe, vbpVerifyGeneric = True}
        in (newLeaders, Just $ getBlockHeader blk, res <> verifyBlock vbp blk)

----------------------------------------------------------------------------
-- SafeCopy instances
----------------------------------------------------------------------------

-- These instances are all gathered at the end because otherwise we'd have to
-- sort types topologically

deriveSafeCopySimple 0 'base ''EpochIndex
deriveSafeCopySimple 0 'base ''LocalSlotIndex
deriveSafeCopySimple 0 'base ''SlotId
deriveSafeCopySimple 0 'base ''Coin
deriveSafeCopySimple 0 'base ''Address
deriveSafeCopySimple 0 'base ''TxIn
deriveSafeCopySimple 0 'base ''TxOut
deriveSafeCopySimple 0 'base ''Tx
deriveSafeCopySimple 0 'base ''FtsSeed

-- Manually written instances can't be derived because
-- 'deriveSafeCopySimple' is not clever enough to add
-- “SafeCopy (Whatever a) =>” constaints.
instance ( SafeCopy (BodyProof b)
         , SafeCopy (ConsensusData b)
         , SafeCopy (ExtraHeaderData b)
         ) =>
         SafeCopy (GenericBlockHeader b) where
    getCopy =
        contain $
        do _gbhPrevBlock <- safeGet
           _gbhBodyProof <- safeGet
           _gbhConsensus <- safeGet
           _gbhExtra <- safeGet
           return $! GenericBlockHeader {..}
    putCopy GenericBlockHeader {..} =
        contain $
        do safePut _gbhPrevBlock
           safePut _gbhBodyProof
           safePut _gbhConsensus
           safePut _gbhExtra

instance ( SafeCopy (BodyProof b)
         , SafeCopy (ConsensusData b)
         , SafeCopy (ExtraHeaderData b)
         , SafeCopy (Body b)
         , SafeCopy (ExtraBodyData b)
         ) =>
         SafeCopy (GenericBlock b) where
    getCopy =
        contain $
        do _gbHeader <- safeGet
           _gbBody <- safeGet
           _gbExtra <- safeGet
           return $! GenericBlock {..}
    putCopy GenericBlock {..} =
        contain $
        do safePut _gbHeader
           safePut _gbBody
           safePut _gbExtra

deriveSafeCopySimple 0 'base ''ChainDifficulty

instance SscTypes ssc => SafeCopy (BodyProof (MainBlockchain ssc)) where
    getCopy =
        contain $
        do mpNumber <- safeGet
           mpRoot <- safeGet
           mpMpcProof <- safeGet
           return $! MainProof {..}
    putCopy MainProof {..} =
        contain $
        do safePut mpNumber
           safePut mpRoot
           safePut mpMpcProof

instance SafeCopy (BodyProof (GenesisBlockchain ssc)) where
    getCopy =
        contain $
        do x <- safeGet
           return $! GenesisProof x
    putCopy (GenesisProof x) =
        contain $
        do safePut x

instance SafeCopy (ConsensusData (MainBlockchain ssc)) where
    getCopy =
        contain $
        do _mcdSlot <- safeGet
           _mcdLeaderKey <- safeGet
           _mcdDifficulty <- safeGet
           _mcdSignature <- safeGet
           return $! MainConsensusData {..}
    putCopy MainConsensusData {..} =
        contain $
        do safePut _mcdSlot
           safePut _mcdLeaderKey
           safePut _mcdDifficulty
           safePut _mcdSignature

instance SafeCopy (ConsensusData (GenesisBlockchain ssc)) where
    getCopy =
        contain $
        do _gcdEpoch <- safeGet
           _gcdDifficulty <- safeGet
           return $! GenesisConsensusData {..}
    putCopy GenesisConsensusData {..} =
        contain $
        do safePut _gcdEpoch
           safePut _gcdDifficulty

instance SscTypes ssc => SafeCopy (Body (MainBlockchain ssc)) where
    getCopy =
        contain $
        do _mbTxs <- safeGet
           _mbMpc <- safeGet
           return $! MainBody {..}
    putCopy MainBody {..} =
        contain $
        do safePut _mbTxs
           safePut _mbMpc

instance SafeCopy (Body (GenesisBlockchain ssc)) where
    getCopy =
        contain $
        do _gbLeaders <- safeGet
           return $! GenesisBody {..}
    putCopy GenesisBody {..} =
        contain $
        do safePut _gbLeaders

----------------------------------------------------------------------------
-- Other derived instances
----------------------------------------------------------------------------

derive makeNFData ''TxOut
