{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Types.Block.Instances
       ( BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       -- * MainBlock and GenesisBlock lenses
       -- Move it from here
       , blockLeaderKey
       , blockLeaders
       , blockMpc
       , blockSignature
       , blockSlot
       , blockTxs
       , blockTxas
       , blockProxySKs

       , gcdDifficulty
       , gcdEpoch
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

       -- * HeaderHash related functions
       , blockHeader
       , blockHeaderHash
       ) where

import           Control.Lens          (Getter, choosing, to)
import           Data.Tagged           (untag)
import           Data.Text.Buildable   (Buildable)
import qualified Data.Text.Buildable   as Buildable
import           Formatting            (bprint, build, int, sformat, stext, (%))
import           Serokell.Util.Text    (listJson)
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.Crypto            (Hash, PublicKey, hash, hashHexF, unsafeHash)
import           Pos.Merkle            (MerkleRoot, MerkleTree, mtRoot)
import           Pos.Ssc.Class.Helpers (SscHelpersClass (..))
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Types.Block.Class (Blockchain (..), GenericBlock (..),
                                        GenericBlockHeader (..), HasPrevBlock (..),
                                        gbBody, gbHeader, gbhConsensus, gbhPrevBlock)
import           Pos.Types.Block.Types (BiHeader, BiSsc, Block, BlockHeader,
                                        BlockSignature, GenesisBlock, GenesisBlockHeader,
                                        GenesisBlockchain, MainBlock, MainBlockHeader,
                                        MainBlockchain, MainExtraBodyData,
                                        MainExtraHeaderData)
import           Pos.Types.Core        (ChainDifficulty, EpochIndex (..),
                                        HasDifficulty (..), HasEpochIndex (..),
                                        HasEpochOrSlot (..), HasHeaderHash (..),
                                        HeaderHash, SlotId (..), slotIdF)
import           Pos.Types.Types       (ProxySKHeavy, SlotLeaders, Tx, TxAux,
                                        TxDistribution, TxWitness)
import           Pos.Update.Core.Types (UpdatePayload, UpdateProof, UpdateProposal,
                                        mkUpdateProof)
import           Pos.Util              (Color (Magenta), colorize)


----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance (SscHelpersClass ssc, Bi TxWitness, Bi UpdatePayload, Bi EpochIndex) =>
         Blockchain (MainBlockchain ssc) where
    -- | Proof of transactions list and MPC data.
    data BodyProof (MainBlockchain ssc) = MainProof
        { mpNumber        :: !Word32
        , mpRoot          :: !(MerkleRoot Tx)
        , mpWitnessesHash :: !(Hash [TxWitness])
        , mpMpcProof      :: !(SscProof ssc)
        , mpProxySKsProof :: !(Hash [ProxySKHeavy])
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
          _mbProxySKs :: ![ProxySKHeavy]
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
    verifyBBlock GenericBlock{..} =
        first pretty $ untag sscVerifyPayload (Right _gbHeader) (_mbMpc _gbBody)

--deriving instance Ssc ssc => Show (SscProof ssc)
--deriving instance Ssc ssc => Eq (SscProof ssc)
deriving instance Ssc ssc => Show (BodyProof (MainBlockchain ssc))
deriving instance Ssc ssc => Eq (BodyProof (MainBlockchain ssc))
deriving instance Ssc ssc => Show (Body (MainBlockchain ssc))
deriving instance (Eq (SscPayload ssc), Ssc ssc) => Eq (Body (MainBlockchain ssc))

instance (Ssc ssc) => NFData (BodyProof (MainBlockchain ssc))
instance (Ssc ssc) => NFData (ConsensusData (MainBlockchain ssc))
instance (Ssc ssc) => NFData (Body (MainBlockchain ssc))
instance (Ssc ssc) => NFData (MainBlock ssc)

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

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
    verifyBBlock _ = pure ()

instance (Ssc ssc) => NFData (BodyProof (GenesisBlockchain ssc))
instance (Ssc ssc) => NFData (ConsensusData (GenesisBlockchain ssc))
instance (Ssc ssc) => NFData (Body (GenesisBlockchain ssc))
instance (Ssc ssc) => NFData (GenesisBlock ssc)

----------------------------------------------------------------------------
-- Lenses. Move it from here
----------------------------------------------------------------------------

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
mbProxySKs :: Lens' (Body (MainBlockchain ssc)) [ProxySKHeavy]
MAKE_LENS(mbProxySKs, _mbProxySKs)

-- | Lens for 'UpdatePayload' in main block body.
mbUpdatePayload :: Lens' (Body (MainBlockchain ssc)) UpdatePayload
MAKE_LENS(mbUpdatePayload, _mbUpdatePayload)

-- makeLensesData ''Body ''(GenesisBlockchain ssc)

-- | Lens for 'SlotLeaders' in 'Body' of 'GenesisBlockchain'.
gbLeaders :: Lens' (Body (GenesisBlockchain ssc)) SlotLeaders
MAKE_LENS(gbLeaders, _gbLeaders)

-- | Lens from 'MainBlockHeader' to 'SlotId'.
headerSlot :: Lens' (MainBlockHeader ssc) SlotId
headerSlot = gbhConsensus . mcdSlot

-- | Lens from 'MainBlockHeader' to 'PublicKey'.
headerLeaderKey :: Lens' (MainBlockHeader ssc) PublicKey
headerLeaderKey = gbhConsensus . mcdLeaderKey

-- | Lens from 'MainBlockHeader' to 'Signature'.
headerSignature :: Lens' (MainBlockHeader ssc) (BlockSignature ssc)
headerSignature = gbhConsensus . mcdSignature

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

-- | Lens from 'MainBlock' to 'ProxySKHeavy' list.
blockProxySKs :: Lens' (MainBlock ssc) [ProxySKHeavy]
blockProxySKs = gbBody . mbProxySKs

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
blockLeaders :: Lens' (GenesisBlock ssc) SlotLeaders
blockLeaders = gbBody . gbLeaders

----------------------------------------------------------------------------
-- Buildable instances for MainBlock and GenesisBlock
----------------------------------------------------------------------------

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

instance BiSsc ssc => Buildable (BlockHeader ssc) where
    build = either Buildable.build Buildable.build

----------------------------------------------------------------------------
-- HasEpochIndex
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- HasEpochOrSlot
----------------------------------------------------------------------------

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
-- HasHeaderHash
----------------------------------------------------------------------------

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

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
blockHeaderHash :: BiHeader ssc => BlockHeader ssc -> HeaderHash
blockHeaderHash = headerHash

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block ssc -> BlockHeader ssc
getBlockHeader = bimap _gbHeader _gbHeader

-- This gives a “redundant constraint” message warning which will be fixed in
-- lens-4.15 (not in LTS yet).
blockHeader :: Getter (Block ssc) (BlockHeader ssc)
blockHeader = to getBlockHeader

----------------------------------------------------------------------------
-- HasDifficulty
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- HasPrevBlock
----------------------------------------------------------------------------

-- | Class for something that has previous block (lens to 'Hash' for this block).
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
