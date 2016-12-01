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
       , FlatSlotId
       , LocalSlotIndex (..)
       , SlotId (..)
       , EpochOrSlot (..)
       , slotIdF
       , epochOrSlot

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

       , SharedSeed (..)
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
       , HasEpochOrSlot (..)

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
       , headerLeaderKey
       , headerSignature
       , headerSlot
       , mbMpc
       , mbTxs
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature
       ) where

import           Control.Lens           (Getter, Lens', choosing, makeLenses, to, view)
import           Data.Aeson             (ToJSON (toJSON))
import           Data.Aeson.TH          (deriveToJSON)
import           Data.Binary            (Binary)
import           Data.Binary.Orphans    ()
import qualified Data.ByteString        as BS (pack, zipWith)
import qualified Data.ByteString.Char8  as BSC (pack)
import           Data.Data              (Data)
import           Data.DeriveTH          (derive, makeNFData)
import           Data.Hashable          (Hashable)
import           Data.Ix                (Ix)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as M (toList)
import           Data.MessagePack       (MessagePack (..))
import           Data.SafeCopy          (SafeCopy (..), base, contain,
                                         deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Semigroup         (Semigroup (..))
import           Data.Tagged            (untag)
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, int, later, ords, sformat,
                                         stext, (%))
import           Serokell.AcidState     ()
import           Serokell.Aeson.Options (defaultOptions)
import qualified Serokell.Util.Base16   as B16
import           Serokell.Util.Text     (listJson, mapBuilderJson, pairBuilder)
import           Universum

import           Pos.Constants          (sharedSeedLength)
import           Pos.Crypto             (Hash, PublicKey, Signature, hash, hashHexF,
                                         shortHashF)
import           Pos.Merkle             (MerkleRoot, MerkleTree, mtRoot, mtSize)
import           Pos.Ssc.Class.Types    (Ssc (..))
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
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Generic, Binary, Hashable, ToJSON)

instance MessagePack EpochIndex

instance Buildable EpochIndex where
    build = bprint ("epoch #"%int)

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Binary, Hashable, Buildable, ToJSON)

instance MessagePack LocalSlotIndex

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic)

instance Binary SlotId
instance MessagePack SlotId

$(deriveToJSON defaultOptions ''SlotId)

instance Buildable SlotId where
    build SlotId {..} =
        bprint (ords%" slot of "%ords%" epoch") siSlot siEpoch

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

-- | Represents SlotId or EpochIndex. Useful because genesis blocks
-- have only EpochIndex, while main blocks have SlotId.
newtype EpochOrSlot = EpochOrSlot
    { unEpochOrSlot :: Either EpochIndex SlotId
    } deriving (Show, Eq)

-- | Apply one of the function depending on content of EpochOrSlot.
epochOrSlot :: (EpochIndex -> a) -> (SlotId -> a) -> EpochOrSlot -> a
epochOrSlot f g = either f g . unEpochOrSlot

instance Ord EpochOrSlot where
    EpochOrSlot (Left e1) < EpochOrSlot (Left e2) = e1 < e2
    EpochOrSlot (Right s1) < EpochOrSlot (Right s2) = s1 < s2
    EpochOrSlot (Right s1) < EpochOrSlot (Left e2) = siEpoch s1 < e2
    EpochOrSlot (Left e1) < EpochOrSlot (Right s2) = e1 <= siEpoch s2
    EpochOrSlot (Left e1) <= EpochOrSlot (Left e2) = e1 <= e2
    EpochOrSlot (Right s1) <= EpochOrSlot (Right s2) = s1 <= s2
    EpochOrSlot a <= EpochOrSlot b = a < b

instance Buildable EpochOrSlot where
    build = either Buildable.build Buildable.build . unEpochOrSlot

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- | Address is where you can send coins.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show, Eq, Generic, Buildable, Ord, Binary, Hashable, NFData, ToJSON)

instance MessagePack Address

-- | Specialized formatter for 'Address'.
addressF :: Format r (Address -> r)
addressF = build

----------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

-- | 'Signature' of addrId.
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

-- | Specialized formatter for 'Tx'.
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

-- | Format 'Utxo' map as json.
formatUtxo :: Utxo -> Builder
formatUtxo = mapBuilderJson . map (first pairBuilder) . M.toList

-- | Specialized formatter for 'Utxo'.
utxoF :: Format r (Utxo -> r)
utxoF = later formatUtxo

----------------------------------------------------------------------------
-- SSC. It means shared seed computation, btw
----------------------------------------------------------------------------

-- | This is a shared seed used for follow-the-satoshi. This seed is
-- randomly generated by each party and eventually they agree on the
-- same value.
newtype SharedSeed = SharedSeed
    { getSharedSeed :: ByteString
    } deriving (Show, Eq, Ord, Generic, Binary, NFData)

instance MessagePack SharedSeed

instance ToJSON SharedSeed where
    toJSON = toJSON . pretty

instance Buildable SharedSeed where
    build = B16.formatBase16 . getSharedSeed

instance Semigroup SharedSeed where
    (<>) (SharedSeed a) (SharedSeed b) =
        SharedSeed $ BS.pack (BS.zipWith xor a b) -- fast due to rewrite rules

instance Monoid SharedSeed where
    mempty = SharedSeed $ BSC.pack $ replicate sharedSeedLength '\NUL'
    mappend = (Data.Semigroup.<>)
    mconcat = foldl' mappend mempty

-- | 'NonEmpty' list of slot leaders.
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

-- | Constraint for data to be signed in main block.
type MainToSign ssc = (HeaderHash ssc, BodyProof (MainBlockchain ssc), SlotId, ChainDifficulty)

instance Ssc ssc => Blockchain (MainBlockchain ssc) where
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

deriving instance Ssc ssc => Eq (BodyProof (MainBlockchain ssc))
deriving instance Ssc ssc => Show (Body (MainBlockchain ssc))

instance Ssc ssc => Binary (BodyProof (MainBlockchain ssc))
instance Ssc ssc => Binary (ConsensusData (MainBlockchain ssc))
instance Ssc ssc => Binary (Body (MainBlockchain ssc))

-- | Header of generic main block.
type MainBlockHeader ssc = GenericBlockHeader (MainBlockchain ssc)

instance Ssc ssc => Buildable (MainBlockHeader ssc) where
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

instance Ssc ssc => Buildable (MainBlock ssc) where
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

-- | Header of Genesis block.
type GenesisBlockHeader ssc = GenericBlockHeader (GenesisBlockchain ssc)

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

-- | Genesis block parametrized by 'GenesisBlockchain'.
type GenesisBlock ssc = GenericBlock (GenesisBlockchain ssc)

instance Ssc ssc => Buildable (GenesisBlock ssc) where
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

instance Ssc ssc => Buildable (GenesisBlockHeader ssc) where
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

-- | Either header of ordinary main block or genesis block.
type BlockHeader ssc = Either (GenesisBlockHeader ssc) (MainBlockHeader ssc)
-- | 'Hash' of block header.
type HeaderHash ssc = Hash (BlockHeader ssc)

-- | Specialized formatter for 'HeaderHash'.
headerHashF :: Format r (HeaderHash ssc -> r)
headerHashF = build

-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

----------------------------------------------------------------------------
-- Lenses. [CSL-193]: move to Block.hs and other modules or leave them here?
----------------------------------------------------------------------------

makeLenses ''GenericBlockHeader
makeLenses ''GenericBlock

-- !!! Create issue about this on lens github or give link on existing issue !!!
-- 'makeLensesData' doesn't work with types with parameters. I don't
-- know how to design a 'makeLensesData' which would work with them (in fact,
-- I don't even know how an invocation of 'makeLensesData' would look like)

#define MAKE_LENS(l, field) l f s = (\y -> s {field = y}) <$> f (field s)

-- makeLensesData ''ConsensusData ''(MainBlockchain ssc)

-- | Lens for 'SlotId' of 'MainBlockchain' in 'ConsensusData'.
mcdSlot :: Lens' (ConsensusData (MainBlockchain ssc)) SlotId
MAKE_LENS(mcdSlot, _mcdSlot)

-- | Lens for 'PublicKey' of 'MainBlockchain' in 'ConsensusData'.
mcdLeaderKey :: Lens' (ConsensusData (MainBlockchain ssc)) PublicKey
MAKE_LENS(mcdLeaderKey, _mcdLeaderKey)

-- | Lens for 'ChainDifficulty' of 'MainBlockchain' in 'ConsensusData'.
mcdDifficulty :: Lens' (ConsensusData (MainBlockchain ssc)) ChainDifficulty
MAKE_LENS(mcdDifficulty, _mcdDifficulty)

-- | Lens for 'Signature' of 'MainBlockchain' in 'ConsensusData'.
mcdSignature :: Lens' (ConsensusData (MainBlockchain ssc)) (Signature (MainToSign ssc))
MAKE_LENS(mcdSignature, _mcdSignature)

-- makeLensesData ''ConsensusData ''(GenesisBlockchain ssc)

-- | Lens for 'EpochIndex' of 'GenesisBlockchain' in 'ConsensusData'.
gcdEpoch :: Lens' (ConsensusData (GenesisBlockchain ssc)) EpochIndex
MAKE_LENS(gcdEpoch, _gcdEpoch)

-- | Lens for 'ChainDifficulty' of 'GenesisBlockchain' in 'ConsensusData'.
gcdDifficulty :: Lens' (ConsensusData (GenesisBlockchain ssc)) ChainDifficulty
MAKE_LENS(gcdDifficulty, _gcdDifficulty)

-- makeLensesData ''Body ''(MainBlockchain ssc)

-- | Lens for 'MerkleTree' in 'Body' of 'MainBlockChain'.
mbTxs :: Lens' (Body (MainBlockchain ssc)) (MerkleTree Tx)
MAKE_LENS(mbTxs, _mbTxs)

-- | Lens for 'MerkleTree' in 'Body' of 'SscPayload'.
mbMpc :: Lens' (Body (MainBlockchain ssc)) (SscPayload ssc)
MAKE_LENS(mbMpc, _mbMpc)

-- makeLensesData ''Body ''(GenesisBlockchain ssc)

-- | Lens for 'SlotLeaders' in 'Body' of 'GenesisBlockchain'.
gbLeaders :: Lens' (Body (GenesisBlockchain ssc)) SlotLeaders
MAKE_LENS(gbLeaders, _gbLeaders)

-- | Lens from 'GenericBlock' to 'BodyProof'.
gbBodyProof :: Lens' (GenericBlock b) (BodyProof b)
gbBodyProof = gbHeader . gbhBodyProof

-- | Lens from 'MainBlockHeader' to 'SlotId'.
headerSlot :: Lens' (MainBlockHeader ssc) SlotId
headerSlot = gbhConsensus . mcdSlot

-- | Lens from 'MainBlockHeader' to 'PublicKey'.
headerLeaderKey :: Lens' (MainBlockHeader ssc) PublicKey
headerLeaderKey = gbhConsensus . mcdLeaderKey

-- | Lens from 'MainBlockHeader' to 'Signature'.
headerSignature :: Lens' (MainBlockHeader ssc) (Signature (MainToSign ssc))
headerSignature = gbhConsensus . mcdSignature

-- | Type class for something that has 'ChainDifficulty'.
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

-- | Class for something that has previous block (lens to 'Hash' for this block).
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

-- | Class for something that has 'HeaderHash'.
class HasHeaderHash a ssc | a -> ssc where
    headerHash :: a -> HeaderHash ssc
    headerHashG :: Getter a (HeaderHash ssc)
    headerHashG = to headerHash

instance Ssc ssc => HasHeaderHash (MainBlockHeader ssc) ssc where
    headerHash = hash . Right

instance Ssc ssc => HasHeaderHash (GenesisBlockHeader ssc) ssc where
    headerHash = hash . Left

instance Ssc ssc => HasHeaderHash (BlockHeader ssc) ssc where
    headerHash = hash

instance Ssc ssc => HasHeaderHash (MainBlock ssc) ssc where
    headerHash = hash . Right . view gbHeader

instance Ssc ssc => HasHeaderHash (GenesisBlock ssc) ssc where
    headerHash = hash . Left  . view gbHeader

instance Ssc ssc => HasHeaderHash (Block ssc) ssc where
    headerHash = hash . getBlockHeader

-- | Class for something that has 'EpochIndex'.
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

-- | Lens from 'MainBlock' to 'SlotId'.
blockSlot :: Lens' (MainBlock ssc) SlotId
blockSlot = gbHeader . headerSlot

-- | Lens from 'MainBlock' to 'PublicKey'.
blockLeaderKey :: Lens' (MainBlock ssc) PublicKey
blockLeaderKey = gbHeader . headerLeaderKey

-- | Lens from 'MainBlock' to 'Signature'.
blockSignature :: Lens' (MainBlock ssc) (Signature (MainToSign ssc))
blockSignature = gbHeader . headerSignature

-- | Lens from 'MainBlock' to 'SscPayload'.
blockMpc :: Lens' (MainBlock ssc) (SscPayload ssc)
blockMpc = gbBody . mbMpc

-- | Lens from 'MainBlock' to 'MerkleTree'.
blockTxs :: Lens' (MainBlock ssc) (MerkleTree Tx)
blockTxs = gbBody . mbTxs

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
blockLeaders :: Lens' (GenesisBlock ssc) SlotLeaders
blockLeaders = gbBody . gbLeaders

-- | Lens from 'Block' to 'BlockHeader'.
--
-- This gives a “redundant constraint” message warning which will be fixed in
-- lens-4.15 (not in LTS yet).
blockHeader :: Getter (Block ssc) (BlockHeader ssc)
blockHeader = to getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block ssc -> BlockHeader ssc
getBlockHeader = bimap (view gbHeader) (view gbHeader)

class HasEpochOrSlot a where
    _getEpochOrSlot :: a -> Either EpochIndex SlotId
    getEpochOrSlot :: a -> EpochOrSlot
    getEpochOrSlot = EpochOrSlot . _getEpochOrSlot
    epochOrSlotG :: Getter a EpochOrSlot
    epochOrSlotG = to getEpochOrSlot

instance HasEpochOrSlot (MainBlockHeader ssc) where
    _getEpochOrSlot = Right . _mcdSlot . _gbhConsensus

instance HasEpochOrSlot (GenesisBlockHeader ssc) where
    _getEpochOrSlot = Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot (MainBlock ssc) where
    _getEpochOrSlot = _getEpochOrSlot . _gbHeader

instance HasEpochOrSlot (GenesisBlock ssc) where
    _getEpochOrSlot = _getEpochOrSlot . _gbHeader

instance (HasEpochOrSlot a, HasEpochOrSlot b) =>
         HasEpochOrSlot (Either a b) where
    _getEpochOrSlot = either _getEpochOrSlot _getEpochOrSlot

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
deriveSafeCopySimple 0 'base ''SharedSeed

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

instance Ssc ssc => SafeCopy (BodyProof (MainBlockchain ssc)) where
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

instance Ssc ssc => SafeCopy (Body (MainBlockchain ssc)) where
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
