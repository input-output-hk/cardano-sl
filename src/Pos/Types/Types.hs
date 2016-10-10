-- {-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Definitions of the most fundamental types.

module Pos.Types.Types
       (
         NodeId (..)
       , nodeF

       , EpochIndex (..)
       , LocalSlotIndex (..)
       , SlotId (..)
       , FlatSlotId

       , Coin (..)
       , coinF

       , Address (..)
       , addressF

       , TxSig
       , TxId
       , TxIn (..)
       , TxOut (..)
       , Tx (..)

       , AddrId
       , Utxo

       , FtsSeed (..)
       , Commitment (..)
       , Opening (..)
       , CommitmentsMap
       , OpeningsMap
       , SharesMap

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

       -- TODO: move it from here to Block.hs
       , mkGenericHeader
       , mkGenericBlock
       , mkMainHeader
       , mkMainBlock

       , verifyGenericHeader
       , verifyHeader

       , Entry (..)
       , Blockkk
       , displayEntry
       ) where

import           Data.Binary          (Binary)
import           Data.Binary.Orphans  ()
import           Data.Hashable        (Hashable)
import           Data.List            (genericLength)
import           Data.SafeCopy        (SafeCopy (..), base, contain, deriveSafeCopySimple,
                                       deriveSafeCopySimpleIndexedType, safeGet, safePut)
import qualified Data.Text            as T (unwords)
import           Data.Text.Buildable  (Buildable)
import qualified Data.Text.Buildable  as Buildable
import           Data.Vector          (Vector)
import           Formatting           (Format, bprint, build, int, sformat, shown, (%))
import qualified Serokell.Util.Base16 as B16
import           Serokell.Util.Verify (VerificationRes (..), verifyGeneric)
import           Universum

import           Pos.Crypto           (EncShare, Hash, PublicKey, SecretKey, SecretProof,
                                       Share, Signature, Signed, VssPublicKey, hash, sign,
                                       toPublic, unsafeHash, verify)
import           Pos.Merkle           (MerkleRoot)
import           Pos.Util             (Raw)

----------------------------------------------------------------------------
-- Node. TODO: do we need it?
----------------------------------------------------------------------------

newtype NodeId = NodeId
    { getNodeId :: Int
    } deriving (Show, Eq, Ord, Enum, Binary)

instance Buildable NodeId where
    build = bprint ("#"%int) . getNodeId

nodeF :: Format r (NodeId -> r)
nodeF = build

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Binary, Hashable, Buildable)

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Binary, Hashable, Buildable)

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Generic)

instance Binary SlotId

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Int64
    } deriving (Num, Enum, Integral, Show, Ord, Real, Generic, Eq, Binary, Bounded)

instance Buildable Coin where
    build = bprint (int%" coin(s)")

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- | Address is where you can send coins.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show, Eq, Generic, Buildable, Ord, Binary)

addressF :: Format r (Address -> r)
addressF = build

----------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------

type TxId = Hash Tx

type TxSig = Signature [TxOut]

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

instance Buildable TxIn where
    build TxIn {..} = bprint ("TxIn ("%build%", "%int%")") txInHash txInIndex

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Show, Generic)

instance Binary TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut ("%build%", "%coinF%")") txOutAddress txOutValue

-- | Transaction.
data Tx = Tx
    { txInputs  :: ![TxIn]   -- ^ Inputs of transaction.
    , txOutputs :: ![TxOut]  -- ^ Outputs of transaction.
    } deriving (Eq, Ord, Show, Generic)

instance Binary Tx

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | 'AddrId' identifies usage of address as output of transaction.
-- Basically, it is tuple of transaction identifier, index in list of outputs
-- and associated value.
type AddrId = (TxId, Word32, Coin)

-- | Unspent transaction outputs.
type Utxo = Map AddrId Address

----------------------------------------------------------------------------
-- MPC
----------------------------------------------------------------------------

-- | This is a random 40-bytes seed used for follow-the-satoshi. This
-- seed is randomly generated by each party and eventually then agree
-- on the same value.
newtype FtsSeed = FtsSeed
    { getFtsSeed :: ByteString
    } deriving (Show, Eq, Ord, Binary)

instance Buildable FtsSeed where
    build = B16.formatBase16 . getFtsSeed

-- | Shares can be used to reconstruct Secret.
-- | Commitment is a message generated during the first stage of
-- MPC. It contains encrypted shares and proof of secret.
data Commitment = Commitment
    { commProof  :: !SecretProof
    , commShares :: !(HashMap VssPublicKey (EncShare))
    } deriving (Show, Eq, Generic)

instance Binary Commitment

-- | Opening reveals message.
-- Maybe we'll need to add something here.
newtype Opening = Opening
    { getOpening :: FtsSeed
    } deriving (Show, Eq, Generic, Binary, Buildable)

type CommitmentsMap = HashMap PublicKey (Signed Commitment)
type OpeningsMap = HashMap PublicKey (Signed Opening)

-- | For each node which generated a 'FtsSeed', the shares map gives
-- us keys and corresponding shares sent to those keys. Specifically, if
-- node identified by PublicKey X has received a share from node
-- identified by PublicKey Y, here's how to get this share: @sharesMap ! X ! Y@.
type SharesMap = HashMap PublicKey (Signed (HashMap PublicKey Share))

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
      gbhPrevBlock :: !(Hash (BBlockHeader b))
    , -- | Proof of body.
      gbhBodyProof :: !(BodyProof b)
    , -- | Consensus data to verify consensus algorithm.
      gbhConsensus :: !(ConsensusData b)
    , -- | Any extra data.
      gbhExtra     :: !(ExtraHeaderData b)
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
    { gbHeader :: !(GenericBlockHeader b)
    , gbBody   :: !(Body b)
    , gbExtra  :: !(ExtraBodyData b)
    }

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with transactions and MPC messages.
data MainBlockchain

type MainBlockHeader = GenericBlockHeader MainBlockchain

-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: Word64
    } deriving (Show, Eq, Ord, Num, Binary)

type MainToSign = (HeaderHash, BodyProof MainBlockchain, SlotId, ChainDifficulty)

instance Blockchain MainBlockchain where
    -- | Proof of transactions list.
    -- TODO: add proof of other stuff.
    data BodyProof MainBlockchain = MainProof
        { mpNumber :: !Word32
        , mpRoot   :: !(MerkleRoot Tx)
        } deriving (Show, Eq, Generic)
    data ConsensusData MainBlockchain = MainConsensusData
        { -- | Id of the slot for which this block was generated.
        mcdSlot       :: !SlotId
        , -- | Public key of slot leader. Maybe later we'll see it is redundant.
        mcdLeaderKey  :: !PublicKey
        , -- | Difficulty of chain ending in this block.
        mcdDifficulty :: !ChainDifficulty
        , -- | Signature given by slot leader.
        mcdSignature  :: !(Signature MainToSign)
        } deriving (Generic)
    type BBlockHeader MainBlockchain = BlockHeader

    -- | In our cryptocurrency, body consists of a list of transactions
    -- and MPC messages.
    data Body MainBlockchain = MainBody
        { -- | Transactions are the main payload.
        -- TODO: consider using Vector or something. Not sure if it will be better.
        -- TODO: probably it should be Merkle tree, not just list.
        mbTxs         :: ![Tx]
        , -- | Commitments are added during the first phase of epoch.
        mbCommitments :: !CommitmentsMap
        , -- | Openings are added during the second phase of epoch.
        mbOpenings    :: !OpeningsMap
        , -- | Decrypted shares to be used in the third phase.
        mbShares      :: !SharesMap
        } deriving (Generic)
    type BBlock MainBlockchain = Block

    mkBodyProof MainBody {..} =
        MainProof { mpNumber = genericLength mbTxs, mpRoot = undefined }

instance Binary (BodyProof MainBlockchain)
instance Binary (ConsensusData MainBlockchain)
instance Binary (Body MainBlockchain)

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock = GenericBlock MainBlockchain

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

-- | Represents blockchain consisting of genesis blocks.  Genesis
-- block doesn't have any special payload and is not strictly
-- necessary. However, it is good idea to store list of leaders
-- explicitely, because calculating it may be expensive operation. For
-- example, it is useful for SPV-clients.
data GenesisBlockchain

type GenesisBlockHeader = GenericBlockHeader GenesisBlockchain

instance Blockchain GenesisBlockchain where
    -- | Proof of GenesisBody is just a hash of slot leaders list.
    -- TODO: do we need a Merkle tree? This list probably won't be large.
    data BodyProof GenesisBlockchain = GenesisProof
        !(Hash (Vector PublicKey))
        deriving (Eq, Generic)
    data ConsensusData GenesisBlockchain = GenesisConsensusData
        { -- | Index of the slot for which this genesis block is relevant.
        gcdEpoch :: !EpochIndex
        } deriving (Generic)
    type BBlockHeader GenesisBlockchain = BlockHeader

    -- | Body of genesis block consists of slot leaders for epoch
    -- associated with this block.
    data Body GenesisBlockchain = GenesisBody
        { gbLeaders :: !(Vector PublicKey)
        } deriving (Show, Generic)
    type BBlock GenesisBlockchain = Block

    mkBodyProof = GenesisProof . hash . gbLeaders

instance Binary (BodyProof GenesisBlockchain)
instance Binary (ConsensusData GenesisBlockchain)
instance Binary (Body GenesisBlockchain)

type GenesisBlock = GenericBlock GenesisBlockchain

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

type BlockHeader = Either GenesisBlockHeader MainBlockHeader
type HeaderHash = Hash BlockHeader

type Block = Either GenesisBlock MainBlock

verifyConsensusLocal :: BlockHeader -> VerificationRes
verifyConsensusLocal (Left _)       = mempty
verifyConsensusLocal (Right header) =
    verifyGeneric
        [ ( verify pk (gbhPrevBlock, gbhBodyProof, slotId, d) sig
          , "can't verify signature")
        ]
  where
    GenericBlockHeader {gbhConsensus = consensus, ..} = header
    pk = mcdLeaderKey consensus
    slotId = mcdSlot consensus
    d = mcdDifficulty consensus
    sig = mcdSignature consensus

----------------------------------------------------------------------------
-- Block.hs. TODO: move it into Block.hs.
-- These functions are here because of GHC bug (trac 12127).
----------------------------------------------------------------------------

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
    { gbhPrevBlock = h
    , gbhBodyProof = proof
    , gbhConsensus = consensus h proof
    , gbhExtra = extra
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
    GenericBlock {gbHeader = header, gbBody = body, gbExtra = extraB}
  where
    header = mkGenericHeader prevHeader body consensus extraH

mkMainHeader
    :: Maybe BlockHeader
    -> SlotId
    -> SecretKey
    -> ChainDifficulty
    -> Body MainBlockchain
    -> MainBlockHeader
mkMainHeader prevHeader slotId sk difficulty body =
    mkGenericHeader prevHeader body consensus ()
  where
    signature prevHash proof = sign sk (prevHash, proof, slotId, difficulty)
    consensus prevHash proof =
        MainConsensusData
        { mcdSlot = slotId
        , mcdLeaderKey = toPublic sk
        , mcdDifficulty = difficulty
        , mcdSignature = signature prevHash proof
        }

mkMainBlock
    :: Maybe BlockHeader
    -> SlotId
    -> SecretKey
    -> ChainDifficulty
    -> Body MainBlockchain
    -> MainBlock
mkMainBlock prevHeader slotId sk difficulty body =
    GenericBlock
    { gbHeader = mkMainHeader prevHeader slotId sk difficulty body
    , gbBody = body
    , gbExtra = ()
    }

-- | Perform cheap checks of GenericBlockHeader, which can be done using only
-- header itself and previous header.
verifyGenericHeader
    :: forall b.
       (Binary (BBlockHeader b))
    => Maybe (BBlockHeader b) -> GenericBlockHeader b -> VerificationRes
verifyGenericHeader prevHeader GenericBlockHeader {..} =
    verifyGeneric [verifyHash]
  where
    prevHash = maybe genesisHash hash prevHeader
    verifyHash =
        ( gbhPrevBlock == prevHash
        , sformat
              ("inconsistent previous hash (expected "%build%", found"%build%")")
              gbhPrevBlock prevHash)

-- | Perform cheap checks of BlockHeader, which can be done using only
-- header itself and previous header.
verifyHeader :: Maybe BlockHeader -> BlockHeader -> VerificationRes
verifyHeader prevHeader h =
    mconcat [verifyConsensusLocal h, verifyCommon]
  where
    verifyCommon =
        either
            (verifyGenericHeader prevHeader)
            (verifyGenericHeader prevHeader)
            h

----------------------------------------------------------------------------
-- Block. Leftover.
----------------------------------------------------------------------------

-- | An entry in a block
data Entry

      -- | Transaction
    = ETx Tx

      -- | Hash of random string U that a node has committed to
    | EUHash NodeId (Hash Raw)
      -- | An encrypted piece of secret-shared U that the first node sent to
      -- the second node (and encrypted with the second node's pubkey)
    | EUShare NodeId NodeId EncShare
      -- | Leaders for a specific epoch
    | ELeaders Int [NodeId]

    deriving (Eq, Ord, Show)

-- | Block
type Blockkk = [Entry]

displayEntry :: Entry -> Text
displayEntry (ETx tx) =
    "transaction " <> show tx
displayEntry (EUHash nid h) =
    sformat (nodeF%"'s commitment = "%shown) nid h
displayEntry (EUShare n_from n_to share) =
    sformat (nodeF%"'s share for "%nodeF%" = "%build) n_from n_to share
displayEntry (ELeaders epoch leaders) =
    sformat ("leaders for epoch "%int%" = "%build)
            epoch
            (T.unwords (map (toS . sformat nodeF) leaders))

----------------------------------------------------------------------------
-- SafeCopy instances
----------------------------------------------------------------------------

-- These instances are all gathered at the end because otherwise we'd have to
-- sort types topologically

deriveSafeCopySimple 0 'base ''NodeId
deriveSafeCopySimple 0 'base ''EpochIndex
deriveSafeCopySimple 0 'base ''LocalSlotIndex
deriveSafeCopySimple 0 'base ''SlotId
deriveSafeCopySimple 0 'base ''Coin
deriveSafeCopySimple 0 'base ''Address
deriveSafeCopySimple 0 'base ''TxIn
deriveSafeCopySimple 0 'base ''TxOut
deriveSafeCopySimple 0 'base ''Tx
deriveSafeCopySimple 0 'base ''FtsSeed
deriveSafeCopySimple 0 'base ''Commitment
deriveSafeCopySimple 0 'base ''Opening

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
        do gbhPrevBlock <- safeGet
           gbhBodyProof <- safeGet
           gbhConsensus <- safeGet
           gbhExtra <- safeGet
           return $! GenericBlockHeader {..}
    putCopy GenericBlockHeader {..} =
        contain $
        do safePut gbhPrevBlock
           safePut gbhBodyProof
           safePut gbhConsensus
           safePut gbhExtra

instance ( SafeCopy (BodyProof b)
         , SafeCopy (ConsensusData b)
         , SafeCopy (ExtraHeaderData b)
         , SafeCopy (Body b)
         , SafeCopy (ExtraBodyData b)
         ) =>
         SafeCopy (GenericBlock b) where
    getCopy =
        contain $
        do gbHeader <- safeGet
           gbBody <- safeGet
           gbExtra <- safeGet
           return $! GenericBlock {..}
    putCopy GenericBlock {..} =
        contain $
        do safePut gbHeader
           safePut gbBody
           safePut gbExtra

deriveSafeCopySimple 0 'base ''ChainDifficulty
deriveSafeCopySimpleIndexedType 0 'base ''BodyProof [''MainBlockchain, ''GenesisBlockchain]
deriveSafeCopySimpleIndexedType 0 'base ''ConsensusData [''MainBlockchain, ''GenesisBlockchain]
deriveSafeCopySimpleIndexedType 0 'base ''Body [''MainBlockchain, ''GenesisBlockchain]

-- Obsolete
deriveSafeCopySimple 0 'base ''Entry
