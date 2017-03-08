{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-- needed for stylish-haskell :(
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
import           Serokell.Util         (Color (Magenta), colorize, listJson)
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.Core              (Blockchain (..), ChainDifficulty, EpochIndex (..),
                                        EpochOrSlot (..), GenericBlock (..),
                                        GenericBlockHeader (..), HasBlockVersion (..),
                                        HasDifficulty (..), HasEpochIndex (..),
                                        HasEpochOrSlot (..), HasHeaderHash (..),
                                        HasPrevBlock (..), HasSoftwareVersion (..),
                                        HeaderHash, IsGenesisHeader, IsHeader,
                                        IsMainHeader (..), ProxySKHeavy, SlotId (..),
                                        SlotLeaders, gbBody, gbHeader, gbhConsensus,
                                        gbhExtra, gbhPrevBlock, slotIdF)
import           Pos.Crypto            (Hash, PublicKey, hash, hashHexF, unsafeHash)
import           Pos.Merkle            (MerkleTree)
import           Pos.Ssc.Class.Helpers (SscHelpersClass (..))
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Txp.Core          (Tx, TxAux, TxDistribution, TxPayload, TxProof,
                                        TxWitness, mkTxProof, txpDistributions, txpTxs,
                                        txpWitnesses)
import           Pos.Types.Block.Types (BiHeader, BiSsc, Block, BlockHeader,
                                        BlockSignature, GenesisBlock, GenesisBlockHeader,
                                        GenesisBlockchain, MainBlock, MainBlockHeader,
                                        MainBlockchain, MainExtraBodyData,
                                        MainExtraHeaderData, mehBlockVersion,
                                        mehSoftwareVersion)
import           Pos.Update.Core.Types (UpdatePayload, UpdateProof, UpdateProposal,
                                        mkUpdateProof)

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance (SscHelpersClass ssc, Bi TxWitness, Bi UpdatePayload, Bi EpochIndex) =>
         Blockchain (MainBlockchain ssc) where
    -- | Proof of transactions list and MPC data.
    data BodyProof (MainBlockchain ssc) = MainProof
        { mpTxProof        :: !TxProof
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
        { -- | Txp payload.
          _mbTxPayload :: !TxPayload
        , -- | Data necessary for MPC.
          _mbMpc :: !(SscPayload ssc)
        , -- | No-ttl heavyweight delegation certificates.
          _mbProxySKs :: ![ProxySKHeavy]
          -- | Additional update information for update system.
        , _mbUpdatePayload :: !UpdatePayload
        } deriving (Generic, Typeable)

    type ExtraBodyData (MainBlockchain ssc) = MainExtraBodyData
    type BBlock (MainBlockchain ssc) = Block ssc

    mkBodyProof MainBody{..} =
        MainProof
        { mpTxProof = mkTxProof _mbTxPayload
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

-- | Lens for transaction payload in main block body.
mbTxPayload :: Lens' (Body (MainBlockchain ssc)) TxPayload
MAKE_LENS(mbTxPayload, _mbTxPayload)

-- | Lens for transaction tree in main block body.
mbTxs :: Lens' (Body (MainBlockchain ssc)) (MerkleTree Tx)
mbTxs = mbTxPayload . txpTxs

-- | Lens for witness list in main block body.
mbWitnesses :: Lens' (Body (MainBlockchain ssc)) [TxWitness]
mbWitnesses = mbTxPayload . txpWitnesses

-- | Lens for distributions list in main block body.
mbTxAddrDistributions :: Lens' (Body (MainBlockchain ssc)) [TxDistribution]
mbTxAddrDistributions = mbTxPayload . txpDistributions

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
            (length txs)
            txs
            (length _mbProxySKs)
            _mbProxySKs
            _mbMpc
            _mbUpdatePayload
            _gbExtra
      where
        MainBody {..} = _gbBody
        txs = _gbBody ^. mbTxs

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
    getEpochOrSlot = EpochOrSlot . Right . _mcdSlot . _gbhConsensus

instance HasEpochOrSlot (GenesisBlockHeader ssc) where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot (MainBlock ssc) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasEpochOrSlot (GenesisBlock ssc) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance (HasEpochOrSlot a, HasEpochOrSlot b) =>
         HasEpochOrSlot (Either a b) where
    getEpochOrSlot = either getEpochOrSlot getEpochOrSlot

----------------------------------------------------------------------------
-- HasHeaderHash
----------------------------------------------------------------------------

instance HasHeaderHash HeaderHash where
    headerHash = identity

{- The story of unnecessary constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All of the instances below have a BiHeader constraint. BiHeader is defined as
“Bi BlockHeader”, which in its turn expands into:

    Bi (Either GenesisBlockHeader MainBlockHeader)

Thus, effectively we require “Bi MainBlockHeader” for the HasHeaderHash
instance of *Genesis*BlockHeader, and vice-versa. This is because hashing of
all headers (MainBlockHeader and GenesisBlockHeader) is done by converting
them to BlockHeader first, so that a header would have the same hash
regardless of whether it's inside a BlockHeader or not.
-}

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

----------------------------------------------------------------------------
-- Has*Version
----------------------------------------------------------------------------

instance HasBlockVersion MainExtraHeaderData where
    blockVersionL = mehBlockVersion
instance HasSoftwareVersion MainExtraHeaderData where
    softwareVersionL = mehSoftwareVersion

instance HasBlockVersion (MainBlockHeader ssc) where
    blockVersionL = gbhExtra . blockVersionL
instance HasSoftwareVersion (MainBlockHeader ssc) where
    softwareVersionL = gbhExtra . softwareVersionL

instance HasBlockVersion (MainBlock ssc) where
    blockVersionL = gbHeader . blockVersionL
instance HasSoftwareVersion (MainBlock ssc) where
    softwareVersionL = gbHeader . softwareVersionL

----------------------------------------------------------------------------
-- IsHeader, IsGenesisHeader, IsMainHeader
----------------------------------------------------------------------------

-- If these constraints seem wrong to you, read “The story of unnecessary
-- constraints” in this file

instance BiHeader ssc => IsHeader (GenesisBlockHeader ssc)
instance BiHeader ssc => IsGenesisHeader (GenesisBlockHeader ssc)

instance BiHeader ssc => IsHeader (MainBlockHeader ssc)
instance BiHeader ssc => IsMainHeader (MainBlockHeader ssc) where
    headerSlotL = headerSlot
    headerLeaderKeyL = headerLeaderKey

instance BiHeader ssc => IsHeader (BlockHeader ssc)
