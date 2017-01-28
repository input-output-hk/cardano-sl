{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Definitions of the most fundamental types.

module Pos.Types.Types
       (
         TxAttributes
       , TxInWitness (..)
       , TxWitness
       , TxDistribution (..)
       , TxSigData
       , TxSig
       , TxId
       , TxIn (..)
       , toPair
       , TxOut (..)
       , txOutStake
       , Tx (..)
       , _txInputs
       , _txOutputs
       , _txAttributes
       , txF
       , txaF
       , TxAux
       , TxOutAux

       , Utxo
       , formatUtxo
       , utxoF

       , TxUndo

       , SharedSeed (..)
       , SlotLeaders

       , ProxySigEpoch
       , ProxySKEpoch
       , ProxySigSimple
       , ProxySKSimple
       , ProxySKEither

       , Blockchain (..)
       , BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       , GenericBlockHeader (..)
       , GenericBlock (..)

       , MainBlockchain
       , MainBlockHeader
       , MainExtraBodyData (..)
       , MainExtraHeaderData (..)
       , BlockHeaderAttributes
       , BlockBodyAttributes
       , BiSsc
       , BlockSignature (..)
       , MainToSign
       , MainBlock

       , GenesisBlockchain
       , GenesisBlockHeader
       , GenesisBlock

       , BlockHeader
       , Block

       -- * HeaderHash related functions
       , blockHeaderHash

       -- * Lenses
       , HasPrevBlock (..)

       , blockHeader
       , blockLeaderKey
       , blockLeaders
       , blockMpc
       , blockSignature
       , blockSlot
       , blockTxs
       , blockTxas
       , blockProxySKs
       , gbBody
       , gbBodyProof
       , gbExtra
       , gbHeader
       , gcdDifficulty
       , gcdEpoch
       , gbhConsensus
       , gbhExtra
       , gbhPrevBlock
       , gbhBodyProof
       , getBlockHeader
       , headerLeaderKey
       , headerSignature
       , headerSlot
       , mbMpc
       , mbTxs
       , mbWitnesses
       , mbProxySKs
       , mbUpdatePayload
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature
       , mehBlockVersion
       , mehSoftwareVersion
       , mehAttributes
       , mebAttributes
       ) where

import           Control.Lens           (Getter, choosing, makeLenses, makeLensesFor, to)
import           Data.DeriveTH          (derive, makeNFData)
import           Data.Hashable          (Hashable)
import qualified Data.Map               as M (toList)
import           Data.Tagged            (untag)
import           Data.Text.Buildable    (Buildable)
import qualified Data.Text.Buildable    as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Data.Vector            (Vector)
import           Formatting             (Format, bprint, build, int, later, sformat,
                                         stext, (%))
import           Serokell.AcidState     ()
import qualified Serokell.Util.Base16   as B16
import           Serokell.Util.Text     (listBuilderJSON, listJson, listJsonIndent,
                                         mapBuilderJson, pairBuilder)
import           Universum

import           Pos.Binary.Address     ()
import           Pos.Binary.Class       (Bi)
import           Pos.Binary.Script      ()
import           Pos.Crypto             (Hash, ProxySecretKey, ProxySignature, PublicKey,
                                         Signature, hash, hashHexF, shortHashF,
                                         unsafeHash)
import           Pos.Data.Attributes    (Attributes)
import           Pos.Merkle             (MerkleRoot, MerkleTree, mtRoot)
import           Pos.Script.Type        (Script)
import           Pos.Ssc.Class.Types    (Ssc (..))
import           Pos.Types.Core         (Address (..), BlockVersion, ChainDifficulty,
                                         Coin, EpochIndex, HasDifficulty (..),
                                         HasEpochIndex (..), HasEpochOrSlot (..),
                                         HasHeaderHash (..), HeaderHash, SlotId (..),
                                         SoftwareVersion, StakeholderId, coinF, slotIdF)
import           Pos.Update.Core.Types  (UpdatePayload, UpdateProof, UpdateProposal,
                                         mkUpdateProof)
import           Pos.Util               (Color (Magenta), colorize)

----------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes ()

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

-- | Data that is being signed when creating a TxSig.
type TxSigData = (TxId, Word32, Hash [TxOut], Hash TxDistribution)

-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

-- | A witness for a single input.
data TxInWitness
    = PkWitness { twKey :: PublicKey
                , twSig :: TxSig}
    | ScriptWitness { twValidator :: Script
                    , twRedeemer  :: Script}
    deriving (Eq, Show, Generic, Typeable)

instance Hashable TxInWitness

instance Bi Script => Buildable TxInWitness where
    build (PkWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (ScriptWitness val red) =
        bprint ("ScriptWitness: "%
                "validator hash = "%shortHashF%", "%
                "redeemer hash = "%shortHashF) (hash val) (hash red)

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness

-- | Distribution of “fake” stake that follow-the-satoshi would use for a
-- particular transaction.
newtype TxDistribution = TxDistribution {
    getTxDistribution :: [[(StakeholderId, Coin)]] }
    deriving (Eq, Show, Generic, Typeable)

instance Buildable TxDistribution where
    build (TxDistribution x) =
        listBuilderJSON . map (listBuilderJSON . map pairBuilder) $ x

-- | Transaction input.
data TxIn = TxIn
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Hashable TxIn

instance Buildable TxIn where
    build TxIn {..} = bprint ("TxIn "%shortHashF%" #"%int) txInHash txInIndex

-- | Make pair from TxIn
toPair :: TxIn -> (TxId, Word32)
toPair (TxIn h i) = (h, i)

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

type TxOutAux = (TxOut, [(StakeholderId, Coin)])

instance Buildable TxOutAux where
    build (out, distr) =
        bprint ("{txout = "%build%", distr = "%listJson%"}")
               out (map pairBuilder distr)

-- | Use this function if you need to know how a 'TxOut' distributes stake
-- (e.g. for the purpose of running follow-the-satoshi).
txOutStake :: TxOutAux -> [(StakeholderId, Coin)]
txOutStake (TxOut{..}, mb) = case txOutAddress of
    PubKeyAddress x -> [(x, txOutValue)]
    ScriptAddress _ -> mb

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = Tx
    { txInputs     :: ![TxIn]   -- ^ Inputs of transaction.
    , txOutputs    :: ![TxOut]  -- ^ Outputs of transaction.
    , txAttributes :: !TxAttributes -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

makeLensesFor [("txInputs", "_txInputs"), ("txOutputs", "_txOutputs")
              , ("txAttributes", "_txAttributes")] ''Tx

-- | Transaction + auxiliary data
type TxAux = (Tx, TxWitness, TxDistribution)

instance Hashable Tx

instance Buildable Tx where
    build Tx {..} =
        bprint
            ("Transaction with inputs "%listJson%", outputs: "%listJson)
            txInputs txOutputs

-- | Specialized formatter for 'Tx'.
txF :: Format r (Tx -> r)
txF = build

-- | Specialized formatter for 'Tx' with auxiliary data
txaF :: Bi Script => Format r (TxAux -> r)
txaF = later $ \(tx, w, d) ->
    bprint (build%"\n"%
            "witnesses: "%listJsonIndent 4%"\n"%
            "distribution: "%build) tx w d

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map (TxId, Word32) TxOutAux

-- | Format 'Utxo' map as json.
formatUtxo :: Utxo -> Builder
formatUtxo =
    mapBuilderJson .
    map (first pairBuilder . second (show @_ @Text)) .
    M.toList

-- | Specialized formatter for 'Utxo'.
utxoF :: Format r (Utxo -> r)
utxoF = later formatUtxo

----------------------------------------------------------------------------
-- UNDO
----------------------------------------------------------------------------

-- | Particular undo needed for transactions
type TxUndo = [[TxOutAux]]

----------------------------------------------------------------------------
-- SSC. It means shared seed computation, btw
----------------------------------------------------------------------------

-- | This is a shared seed used for follow-the-satoshi. This seed is
-- randomly generated by each party and eventually they agree on the
-- same value.
newtype SharedSeed = SharedSeed
    { getSharedSeed :: ByteString
    } deriving (Show, Eq, Ord, Generic, NFData, Typeable)

instance Buildable SharedSeed where
    build = B16.formatBase16 . getSharedSeed

-- | 'NonEmpty' list of slot leaders.
type SlotLeaders = NonEmpty StakeholderId

----------------------------------------------------------------------------
-- Proxy signatures and delegation
----------------------------------------------------------------------------

-- | Proxy signature used in csl -- holds a pair of epoch
-- indices. Block is valid if it's epoch index is inside this range.
type ProxySigEpoch a = ProxySignature (EpochIndex, EpochIndex) a

-- | Same alias for the proxy secret key (see 'ProxySigEpoch').
type ProxySKEpoch = ProxySecretKey (EpochIndex, EpochIndex)

-- | Simple proxy signature without ttl/epoch index constraints.
type ProxySigSimple a = ProxySignature () a

-- | Correspondent SK for no-ttl proxy signature scheme.
type ProxySKSimple = ProxySecretKey ()

-- | Some proxy secret key.
type ProxySKEither = Either ProxySKEpoch ProxySKSimple

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
    -- | Hash of 'BBlockHeader'. This is something like @Hash (BBlockHeader p)@.
    type BHeaderHash p :: *
    type BHeaderHash p = HeaderHash

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
      _gbhPrevBlock :: !(BHeaderHash b)
    , -- | Proof of body.
      _gbhBodyProof :: !(BodyProof b)
    , -- | Consensus data to verify consensus algorithm.
      _gbhConsensus :: !(ConsensusData b)
    , -- | Any extra data.
      _gbhExtra     :: !(ExtraHeaderData b)
    } deriving (Generic)

deriving instance ( Show (BHeaderHash b)
                  , Show (BodyProof b)
                  , Show (ConsensusData b)
                  , Show (ExtraHeaderData b)
                  ) => Show (GenericBlockHeader b)

deriving instance ( Eq (BHeaderHash b)
                  , Eq (BodyProof b)
                  , Eq (ConsensusData b)
                  , Eq (ExtraHeaderData b)
                  ) => Eq (GenericBlockHeader b)

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

deriving instance ( Eq (BHeaderHash b)
                  , Eq (Body b)
                  , Eq (BodyProof b)
                  , Eq (ConsensusData b)
                  , Eq (ExtraBodyData b)
                  , Eq (ExtraHeaderData b)
                  ) => Eq (GenericBlock b)

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with transactions and MPC messages.
data MainBlockchain ssc

-- | Data to be signed in main block.
type MainToSign ssc = (HeaderHash, BodyProof (MainBlockchain ssc), SlotId, ChainDifficulty)

-- | Signature of the block. Can be either regular signature from the
-- issuer or delegated signature having a constraint on epoch indices
-- (it means the signature is valid only if block's slot id has epoch
-- inside the constrained interval).
data BlockSignature ssc
    = BlockSignature (Signature (MainToSign ssc))
    | BlockPSignatureEpoch (ProxySigEpoch (MainToSign ssc))
    | BlockPSignatureSimple (ProxySigSimple (MainToSign ssc))
    deriving (Show, Eq)

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
    }
    deriving (Eq, Show, Generic)

instance Buildable MainExtraHeaderData where
    build MainExtraHeaderData {..} =
      bprint ( "    block: v"%build%"\n"
             % "    software: "%build%"\n"
             )
            _mehBlockVersion
            _mehSoftwareVersion

-- | Represents main block extra data
newtype MainExtraBodyData = MainExtraBodyData
    { _mebAttributes  :: BlockBodyAttributes
    } deriving (Eq, Show, Generic)

instance Buildable MainExtraBodyData where
    -- Currently there is no extra data in block body, attributes are empty.
    build _ = bprint "no extra data"

instance (Ssc ssc, Bi TxWitness, Bi UpdatePayload) =>
         Blockchain (MainBlockchain ssc) where
    -- | Proof of transactions list and MPC data.
    data BodyProof (MainBlockchain ssc) = MainProof
        { mpNumber        :: !Word32
        , mpRoot          :: !(MerkleRoot Tx)
        , mpWitnessesHash :: !(Hash [TxWitness])
        , mpMpcProof      :: !(SscProof ssc)
        , mpProxySKsProof :: !(Hash [ProxySKSimple])
        , mpUpdateProof   :: !UpdateProof
        } deriving (Generic)
    data ConsensusData (MainBlockchain ssc) = MainConsensusData
        { -- | Id of the slot for which this block was generated.
        _mcdSlot       :: !SlotId
        , -- | Public key of slot leader. Maybe later we'll see it is redundant.
        _mcdLeaderKey  :: !PublicKey
        , -- | Difficulty of chain ending in this block.
        _mcdDifficulty :: !ChainDifficulty
        , -- | Signature given by slot leader.
        _mcdSignature  :: !(BlockSignature ssc)
        } deriving (Generic, Show, Eq)
    type BBlockHeader (MainBlockchain ssc) = BlockHeader ssc
    type ExtraHeaderData (MainBlockchain ssc) = MainExtraHeaderData

    -- | In our cryptocurrency, body consists of a list of transactions
    -- and MPC messages.
    data Body (MainBlockchain ssc) = MainBody
        { -- | Transactions are the main payload.
          -- TODO: currently we don't know for sure whether it should be
          -- serialized as a MerkleTree or something list-like.
          _mbTxs :: !(MerkleTree Tx)
        , -- | Distributions for P2SH addresses in transaction outputs.
          --     * length mbTxAddrDistributions == length mbTxs
          --     * i-th element is 'Just' if at least one output of i-th
          --         transaction is P2SH
          --     * n-th element of i-th element is 'Just' if n-th output
          --         of i-th transaction is P2SH
          -- Ask @neongreen if you don't understand wtf is going on.
          -- Basically, address distributions are needed so that (potential)
          -- receivers of P2SH funds would count as stakeholders.
          _mbTxAddrDistributions :: ![TxDistribution]
        , -- | Transaction witnesses. Invariant: there are as many witnesses
          -- as there are transactions in the block. This is checked during
          -- deserialisation. We can't put them into the same Merkle tree
          -- with transactions, as the whole point of segwit is to separate
          -- transactions and witnesses.
          --
          -- TODO: should they be put into a separate Merkle tree or left as
          -- a list?
          _mbWitnesses :: ![TxWitness]
        , -- | Data necessary for MPC.
          _mbMpc :: !(SscPayload ssc)
        , -- | No-ttl heavyweight delegation certificates
          _mbProxySKs :: ![ProxySKSimple]
          -- | Additional update information for update system.
        , _mbUpdatePayload :: !UpdatePayload
        } deriving (Generic, Typeable)

    type ExtraBodyData (MainBlockchain ssc) = MainExtraBodyData
    type BBlock (MainBlockchain ssc) = Block ssc

    mkBodyProof MainBody{..} =
        MainProof
        { mpNumber = fromIntegral (length _mbTxs)
        , mpRoot = mtRoot _mbTxs
        , mpWitnessesHash = hash _mbWitnesses
        , mpMpcProof = untag @ssc mkSscProof _mbMpc
        , mpProxySKsProof = hash _mbProxySKs
        , mpUpdateProof = mkUpdateProof _mbUpdatePayload
        }


--deriving instance Ssc ssc => Show (SscProof ssc)
--deriving instance Ssc ssc => Eq (SscProof ssc)
deriving instance Ssc ssc => Show (BodyProof (MainBlockchain ssc))
deriving instance Ssc ssc => Eq (BodyProof (MainBlockchain ssc))
deriving instance Ssc ssc => Show (Body (MainBlockchain ssc))
deriving instance (Eq (SscPayload ssc), Ssc ssc) => Eq (Body (MainBlockchain ssc))

-- | Header of generic main block.
type MainBlockHeader ssc = GenericBlockHeader (MainBlockchain ssc)

-- | Ssc w/ buildable blockchain
type BiSsc ssc =
    ( Ssc ssc
    , Bi (GenericBlockHeader (GenesisBlockchain ssc))
    , Bi (GenericBlockHeader (MainBlockchain ssc)))

instance BiSsc ssc => Buildable (MainBlockHeader ssc) where
    build gbh@GenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    leader: "%build%"\n"%
             "    difficulty: "%int%"\n"%
             build
            )
            gbhHeaderHash
            _gbhPrevBlock
            _mcdSlot
            _mcdLeaderKey
            _mcdDifficulty
            _gbhExtra
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ Right gbh
        MainConsensusData {..} = _gbhConsensus

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock ssc = GenericBlock (MainBlockchain ssc)

instance (Bi UpdateProposal, BiSsc ssc) => Buildable (MainBlock ssc) where
    build GenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions ("%int%" items): "%listJson%"\n"%
             "  certificates ("%int%" items): "%listJson%"\n"%
             build%"\n"%
             "  update payload: "%build%"\n"%
             "  "%build
            )
            (colorize Magenta "MainBlock")
            _gbHeader
            (length _mbTxs)
            _mbTxs
            (length _mbProxySKs)
            _mbProxySKs
            _mbMpc
            _mbUpdatePayload
            _gbExtra
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
        } deriving (Generic, Show, Eq)
    type BBlockHeader (GenesisBlockchain ssc) = BlockHeader ssc

    -- | Body of genesis block consists of slot leaders for epoch
    -- associated with this block.
    data Body (GenesisBlockchain ssc) = GenesisBody
        { _gbLeaders :: !SlotLeaders
        } deriving (Generic, Show, Eq)
    type BBlock (GenesisBlockchain ssc) = Block ssc

    mkBodyProof = GenesisProof . hash . _gbLeaders

-- | Genesis block parametrized by 'GenesisBlockchain'.
type GenesisBlock ssc = GenericBlock (GenesisBlockchain ssc)

instance BiSsc ssc => Buildable (GenesisBlock ssc) where
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

instance BiSsc ssc => Buildable (GenesisBlockHeader ssc) where
    build gbh@GenericBlockHeader {..} =
        bprint
            ("GenesisBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    epoch: "%build%"\n"%
             "    difficulty: "%int%"\n"
            )
            gbhHeaderHash
            _gbhPrevBlock
            _gcdEpoch
            _gcdDifficulty
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ Left gbh
        GenesisConsensusData {..} = _gbhConsensus

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Either header of ordinary main block or genesis block.
type BlockHeader ssc = Either (GenesisBlockHeader ssc) (MainBlockHeader ssc)

instance BiSsc ssc => Buildable (BlockHeader ssc) where
    build = either Buildable.build Buildable.build

-- | Lens from 'Block' to 'BlockHeader'.
--
-- This gives a “redundant constraint” message warning which will be fixed in
-- lens-4.15 (not in LTS yet).
blockHeader :: Getter (Block ssc) (BlockHeader ssc)
blockHeader = to getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block ssc -> BlockHeader ssc
getBlockHeader = bimap _gbHeader _gbHeader

type BiHeader ssc = Bi (BlockHeader ssc)

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
blockHeaderHash :: BiHeader ssc => BlockHeader ssc -> HeaderHash
blockHeaderHash = headerHash

instance HasHeaderHash HeaderHash where
    headerHash = identity

instance BiHeader ssc =>
         HasHeaderHash (MainBlockHeader ssc) where
    headerHash = blockHeaderHash . Right

instance BiHeader ssc =>
         HasHeaderHash (GenesisBlockHeader ssc) where
    headerHash = blockHeaderHash . Left

instance BiHeader ssc =>
         HasHeaderHash (BlockHeader ssc) where
    headerHash = unsafeHash

instance BiHeader ssc =>
         HasHeaderHash (MainBlock ssc) where
    headerHash = blockHeaderHash . Right . _gbHeader

instance BiHeader ssc =>
         HasHeaderHash (GenesisBlock ssc) where
    headerHash = blockHeaderHash . Left . _gbHeader

instance BiHeader ssc =>
         HasHeaderHash (Block ssc) where
    headerHash = blockHeaderHash . getBlockHeader

-- | Block.
type Block ssc = Either (GenesisBlock ssc) (MainBlock ssc)

----------------------------------------------------------------------------
-- Lenses. [CSL-193]: move to Block.hs and other modules or leave them here?
----------------------------------------------------------------------------

makeLenses ''GenericBlockHeader
makeLenses ''GenericBlock

makeLenses ''MainExtraHeaderData
makeLenses ''MainExtraBodyData

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
mcdSignature :: Lens' (ConsensusData (MainBlockchain ssc)) (BlockSignature ssc)
MAKE_LENS(mcdSignature, _mcdSignature)

-- makeLensesData ''ConsensusData ''(GenesisBlockchain ssc)

-- | Lens for 'EpochIndex' of 'GenesisBlockchain' in 'ConsensusData'.
gcdEpoch :: Lens' (ConsensusData (GenesisBlockchain ssc)) EpochIndex
MAKE_LENS(gcdEpoch, _gcdEpoch)

-- | Lens for 'ChainDifficulty' of 'GenesisBlockchain' in 'ConsensusData'.
gcdDifficulty :: Lens' (ConsensusData (GenesisBlockchain ssc)) ChainDifficulty
MAKE_LENS(gcdDifficulty, _gcdDifficulty)

-- makeLensesData ''Body ''(MainBlockchain ssc)

-- | Lens for transaction tree in main block body.
mbTxs :: Lens' (Body (MainBlockchain ssc)) (MerkleTree Tx)
MAKE_LENS(mbTxs, _mbTxs)

-- | Lens for witness list in main block body.
mbWitnesses :: Lens' (Body (MainBlockchain ssc)) [TxWitness]
MAKE_LENS(mbWitnesses, _mbWitnesses)

-- | Lens for distributions list in main block body.
mbTxAddrDistributions :: Lens' (Body (MainBlockchain ssc)) [TxDistribution]
MAKE_LENS(mbTxAddrDistributions, _mbTxAddrDistributions)

-- | Lens for 'SscPayload' in main block body.
mbMpc :: Lens' (Body (MainBlockchain ssc)) (SscPayload ssc)
MAKE_LENS(mbMpc, _mbMpc)

-- | Lens for ProxySKs in main block body.
mbProxySKs :: Lens' (Body (MainBlockchain ssc)) [ProxySKSimple]
MAKE_LENS(mbProxySKs, _mbProxySKs)

-- | Lens for 'UpdatePayload' in main block body.
mbUpdatePayload :: Lens' (Body (MainBlockchain ssc)) UpdatePayload
MAKE_LENS(mbUpdatePayload, _mbUpdatePayload)

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
headerSignature :: Lens' (MainBlockHeader ssc) (BlockSignature ssc)
headerSignature = gbhConsensus . mcdSignature

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
class HasPrevBlock s where
    prevBlockL :: Lens' s HeaderHash

instance HasPrevBlock s => HasPrevBlock (s, z) where
    prevBlockL = _1 . prevBlockL

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlockHeader b) where
    prevBlockL = gbhPrevBlock

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlock b) where
    prevBlockL = gbHeader . gbhPrevBlock

instance (HasPrevBlock s, HasPrevBlock s') =>
         HasPrevBlock (Either s s') where
    prevBlockL = choosing prevBlockL prevBlockL

instance HasEpochIndex SlotId where
    epochIndexL f SlotId {..} = (\a -> SlotId {siEpoch = a, ..}) <$> f siEpoch

instance HasEpochIndex (MainBlock ssc) where
    epochIndexL = gbHeader . gbhConsensus . mcdSlot . epochIndexL

instance HasEpochIndex (MainBlockHeader ssc) where
    epochIndexL = gbhConsensus . mcdSlot . epochIndexL

instance HasEpochIndex (GenesisBlock ssc) where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance HasEpochIndex (GenesisBlockHeader ssc) where
    epochIndexL = gbhConsensus . gcdEpoch

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
blockSignature :: Lens' (MainBlock ssc) (BlockSignature ssc)
blockSignature = gbHeader . headerSignature

-- | Lens from 'MainBlock' to 'SscPayload'.
blockMpc :: Lens' (MainBlock ssc) (SscPayload ssc)
blockMpc = gbBody . mbMpc

-- | Lens from 'MainBlock' to 'MerkleTree'.
blockTxs :: Lens' (MainBlock ssc) (MerkleTree Tx)
blockTxs = gbBody . mbTxs

-- | Getter from 'MainBlock' to a list of transactions together with
-- auxiliary data.
blockTxas :: Getter (MainBlock ssc) [TxAux]
blockTxas =
    gbBody .
    to (\b -> zip3 (toList (b ^. mbTxs))
                   (b ^. mbWitnesses)
                   (b ^. mbTxAddrDistributions))

-- | Lens from 'MainBlock' to 'ProxySKSimple' list.
blockProxySKs :: Lens' (MainBlock ssc) [ProxySKSimple]
blockProxySKs = gbBody . mbProxySKs

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
blockLeaders :: Lens' (GenesisBlock ssc) SlotLeaders
blockLeaders = gbBody . gbLeaders

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
-- Other derived instances
----------------------------------------------------------------------------

derive makeNFData ''TxIn
derive makeNFData ''TxInWitness
derive makeNFData ''TxOut
derive makeNFData ''TxDistribution
derive makeNFData ''Tx
