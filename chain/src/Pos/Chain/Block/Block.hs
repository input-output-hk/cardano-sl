{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- for the Getter instances

module Pos.Chain.Block.Block
       ( Block
       , getBlockHeader
       , blockHeader
       , verifyBlockInternal

       -- * GenericBlock
       , GenericBlock
       , mkGenericBlockUnsafe
       , gbHeader
       , gbBody
       , gbExtra
       , gbPrevBlock
       , gbBodyProof
       , gbConsensus

       -- * GenesisBlock
       , GenesisBlock
       , mkGenesisBlock
       , genesisBlock0
       , genBlockPrevBlock
       , genBlockProof
       , genBlockEpoch
       , genBlockDifficulty
       , genBlockHeaderAttributes
       , genBlockLeaders
       , genBlockAttributes
       , verifyGenesisBlock

       -- * MainBlock
       , MainBlock
       , mkMainBlock
       , mkMainBlockExplicit
       , mainBlockPrevBlock
       , mainBlockProof
       , mainBlockSlot
       , mainBlockLeaderKey
       , mainBlockDifficulty
       , mainBlockSignature
       , mainBlockBlockVersion
       , mainBlockSoftwareVersion
       , mainBlockHeaderAttributes
       , mainBlockEBDataProof
       , mainBlockTxPayload
       , mainBlockSscPayload
       , mainBlockDlgPayload
       , mainBlockUpdatePayload
       , mainBlockAttributes
       , verifyMainBlock
       ) where

import           Universum

import           Control.Lens (Getter, choosing, makeLenses, to)
import           Control.Monad.Except (MonadError (throwError))
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import           Formatting (bprint, build, int, sformat, stext, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Chain.Block.Genesis (GenesisBody (..),
                     GenesisBodyAttributes, GenesisConsensusData (..),
                     GenesisExtraBodyData (..), GenesisExtraHeaderData (..),
                     GenesisHeaderAttributes, GenesisProof (..),
                     checkGenesisProof, gbLeaders, gcdEpoch, gebAttributes)
import           Pos.Chain.Block.HasPrevBlock (HasPrevBlock (..))
import           Pos.Chain.Block.Header (BlockHeader (..), BlockSignature (..),
                     GenericBlockHeader, HasHeaderHash (..), HeaderHash,
                     MainConsensusData (..), blockHeaderHash, gbhBodyProof,
                     gbhConsensus, gbhPrevBlock, genHeaderAttributes,
                     genHeaderDifficulty, genHeaderEpoch, genHeaderProof,
                     mainHeaderAttributes, mainHeaderBlockVersion,
                     mainHeaderDifficulty, mainHeaderEBDataProof,
                     mainHeaderLeaderKey, mainHeaderProof, mainHeaderSignature,
                     mainHeaderSlot, mainHeaderSoftwareVersion,
                     mkGenesisHeader, mkMainHeaderExplicit,
                     verifyMainBlockHeader)
import           Pos.Chain.Block.Main (BlockBodyAttributes,
                     BlockHeaderAttributes, MainBody (..),
                     MainExtraBodyData (..), MainExtraHeaderData (..),
                     MainProof (..), checkMainProof, mbDlgPayload,
                     mbSscPayload, mbTxPayload, mbTxs, mbUpdatePayload,
                     mebAttributes, verifyMainBody)
import           Pos.Chain.Delegation.HeavyDlgIndex (ProxySKBlockInfo)
import           Pos.Chain.Delegation.Payload (DlgPayload)
import           Pos.Chain.Genesis.Config as Genesis (Config (..))
import           Pos.Chain.Genesis.Hash (GenesisHash (..))
import           Pos.Chain.Ssc.Functions (verifySscPayload)
import           Pos.Chain.Ssc.Payload (SscPayload)
import           Pos.Chain.Txp.TxPayload (TxPayload)
import           Pos.Chain.Update (ConsensusEra (..))
import           Pos.Chain.Update.BlockVersion (BlockVersion,
                     HasBlockVersion (..))
import           Pos.Chain.Update.Payload (UpdatePayload)
import           Pos.Chain.Update.SoftwareVersion (HasSoftwareVersion (..),
                     SoftwareVersion)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.Common (ChainDifficulty, HasDifficulty (..),
                     SlotLeaders, slotLeadersF)
import           Pos.Core.Slotting (EpochIndex, HasEpochIndex (..),
                     HasEpochOrSlot (..), SlotId (..))
import           Pos.Crypto (Hash, ProtocolMagic, PublicKey, SecretKey, hash)
import           Pos.Util.Some (Some (..))


--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

type Block = Either GenesisBlock MainBlock

instance HasHeaderHash Block where
    headerHash = blockHeaderHash . getBlockHeader

instance HasDifficulty Block where
    difficultyL = choosing difficultyL difficultyL

blockHeader :: Getter Block BlockHeader
blockHeader = to getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block -> BlockHeader
getBlockHeader = \case
    Left  gb -> BlockHeaderGenesis (_gbHeader gb)
    Right mb -> BlockHeaderMain    (_gbHeader mb)

-- | Verify a Block in isolation.
verifyBlockInternal
    :: MonadError Text m
    => Genesis.Config
    -> ConsensusEra
    -> Block
    -> m ()
verifyBlockInternal genesisConfig era =
    either verifyGenesisBlock (verifyMainBlock genesisConfig era)


--------------------------------------------------------------------------------
-- GenericBlock
--------------------------------------------------------------------------------

-- | In general Block consists of header and body. It may contain
-- extra data as well.
data GenericBlock bodyProof consensus extraH body extraB = GenericBlock
    { _gbHeader :: !(GenericBlockHeader bodyProof consensus extraH)
    , _gbBody   :: !body
    , _gbExtra  :: !extraB
    } deriving (Eq, Show, Generic, NFData)

instance
    (Bi bodyProof , Bi consensus, Bi extraH, Bi body, Bi extraB)
    => Bi (GenericBlock bodyProof consensus extraH body extraB)
  where
    encode gb =  encodeListLen 3
              <> encode (_gbHeader gb)
              <> encode (_gbBody gb)
              <> encode (_gbExtra gb)
    decode = do
        enforceSize "GenericBlock" 3
        _gbHeader <- decode
        _gbBody   <- decode
        _gbExtra  <- decode
        pure GenericBlock {..}

instance
    ( SafeCopy bodyProof
    , SafeCopy consensus
    , SafeCopy extraH
    , SafeCopy body
    , SafeCopy extraB
    )
    => SafeCopy (GenericBlock bodyProof consensus extraH body extraB)
  where
    getCopy = contain $ do
        _gbHeader <- safeGet
        _gbBody <- safeGet
        _gbExtra <- safeGet
        return $! GenericBlock {..}
    putCopy GenericBlock {..} = contain $ do
        safePut _gbHeader
        safePut _gbBody
        safePut _gbExtra

mkGenericBlockUnsafe
    :: GenericBlockHeader bodyProof consensus extraH
    -> body
    -> extraB
    -> GenericBlock bodyProof consensus extraH body extraB
mkGenericBlockUnsafe = GenericBlock


----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

type GenesisBlock = GenericBlock
    GenesisProof
    GenesisConsensusData
    GenesisExtraHeaderData
    GenesisBody
    GenesisExtraBodyData

instance Buildable GenesisBlock where
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
            ("  leaders: "%slotLeadersF%"\n") (toList _gbLeaders)

instance HasEpochOrSlot GenesisBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasHeaderHash GenesisBlock where
    headerHash = blockHeaderHash . BlockHeaderGenesis . _gbHeader

-- | Smart constructor for 'GenesisBlock'.
mkGenesisBlock
    :: ProtocolMagic
    -> Either GenesisHash BlockHeader
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock
mkGenesisBlock pm prevHeader epoch leaders = GenericBlock header body extra
  where
    header = mkGenesisHeader pm prevHeader epoch body
    body = GenesisBody leaders
    extra = GenesisExtraBodyData $ mkAttributes ()

-- | Creates the very first genesis block.
genesisBlock0 :: ProtocolMagic -> GenesisHash -> SlotLeaders -> GenesisBlock
genesisBlock0 pm genesisHash leaders = mkGenesisBlock pm (Left genesisHash) 0 leaders

-- | To verify a genesis block we only have to check the body proof.
verifyGenesisBlock
    :: MonadError Text m
    => GenesisBlock
    -> m ()
verifyGenesisBlock GenericBlock {..} =
    checkGenesisProof _gbBody (_gbHeader ^. gbhBodyProof)


----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock = GenericBlock
    MainProof
    MainConsensusData
    MainExtraHeaderData
    MainBody
    MainExtraBodyData

instance HasEpochOrSlot MainBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasHeaderHash MainBlock where
    headerHash = blockHeaderHash . BlockHeaderMain . _gbHeader

-- | Smart constructor for 'MainBlock'.
mkMainBlock
    :: ProtocolMagic
    -> BlockVersion
    -> SoftwareVersion
    -> Either GenesisHash BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainBlock
mkMainBlock pm bv sv prevHeader = mkMainBlockExplicit pm bv sv prevHash difficulty
  where
    prevHash = either getGenesisHash headerHash prevHeader
    difficulty = either (const 0) (succ . view difficultyL) prevHeader

-- | Smart constructor for 'MainBlock', without requiring the entire previous
-- 'BlockHeader'. Instead, you give its hash and the difficulty of this block.
-- These are derived from the previous header in 'mkMainBlock' so if you have
-- the previous header, consider using that one.
mkMainBlockExplicit
    :: ProtocolMagic
    -> BlockVersion
    -> SoftwareVersion
    -> HeaderHash
    -> ChainDifficulty
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainBlock
mkMainBlockExplicit pm bv sv prevHash difficulty slotId sk pske body =
    GenericBlock
        (mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extraH)
        body
        extraB
  where
    extraB :: MainExtraBodyData
    extraB = MainExtraBodyData (mkAttributes ())
    extraH :: MainExtraHeaderData
    extraH =
        MainExtraHeaderData
            bv
            sv
            (mkAttributes ())
            (hash extraB)

verifyMainBlock
    :: MonadError Text m
    => Genesis.Config
    -> ConsensusEra
    -> MainBlock
    -> m ()
verifyMainBlock genesisConfig era GenericBlock {..} = do
    let pm = configProtocolMagic genesisConfig
    verifyMainBlockHeader pm _gbHeader
    verifyMainBody pm _gbBody
    -- No need to verify the main extra body data. It's an 'Attributes ()'
    -- which is valid whenever it's well-formed.
    --
    -- Check internal consistency: the body proofs are all correct.
    checkMainProof _gbBody (_gbHeader ^. gbhBodyProof)
    -- Check that the headers' extra body data hash is correct.
    -- This isn't subsumed by the body proof check.
    unless (hash (_gbExtra) == (_gbHeader ^. mainHeaderEBDataProof)) $
        throwError "Hash of extra body data is not equal to its representation in the header."
    -- Ssc and Dlg consistency checks which require the header, and so can't
    -- be done in 'verifyMainBody'.
    case era of
        Original ->
            either (throwError . pretty) pure $
                verifySscPayload
                    genesisConfig
                    (Right (Some _gbHeader))
                    (_mbSscPayload _gbBody)
        OBFT -> pure () -- We don't perform SSC operations during the OBFT era


----------------------------------------------------------------------------
-- Generic Block Lenses
---------------------------------------------------------------------------

makeLenses ''GenericBlock

-- | Lens from 'GenericBlock' to 'BHeaderHash' of its parent.
gbPrevBlock :: Lens' (GenericBlock a b c d e) HeaderHash
gbPrevBlock = gbHeader . gbhPrevBlock

-- | Lens from 'GenericBlock' to 'BodyProof'.
gbBodyProof :: Lens' (GenericBlock bodyProof b c d e) bodyProof
gbBodyProof = gbHeader . gbhBodyProof

-- | Lens from 'GenericBlock' to 'ConsensusData'.
gbConsensus :: Lens' (GenericBlock a consensus c d e) consensus
gbConsensus = gbHeader . gbhConsensus

instance HasPrevBlock (GenericBlock bodyProof consensus extraH body extraB) where
    prevBlockL = gbHeader . gbhPrevBlock


----------------------------------------------------------------------------
-- GenesisBlock lenses
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlock' to 'HeaderHash' of its parent.
genBlockPrevBlock :: Lens' GenesisBlock HeaderHash
genBlockPrevBlock = gbPrevBlock

-- | Lens from 'GenesisBlock' to 'GenesisProof'.
genBlockProof :: Lens' GenesisBlock GenesisProof
genBlockProof = gbHeader . genHeaderProof

-- | Lens from 'GenesisBlock' to 'EpochIndex'.
genBlockEpoch :: Lens' GenesisBlock EpochIndex
genBlockEpoch = gbHeader . genHeaderEpoch

-- | Lens from 'GenesisBlock' to 'ChainDifficulty'.
genBlockDifficulty :: Lens' GenesisBlock ChainDifficulty
genBlockDifficulty = gbHeader . genHeaderDifficulty

-- | Lens from 'GenesisBlock' to 'GenesisHeaderAttributes'.
genBlockHeaderAttributes :: Lens' GenesisBlock GenesisHeaderAttributes
genBlockHeaderAttributes = gbHeader . genHeaderAttributes

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
genBlockLeaders :: Lens' GenesisBlock SlotLeaders
genBlockLeaders = gbBody . gbLeaders

-- | Lens from 'GenesisBlock' to 'GenesisBodyAttributes'.
genBlockAttributes :: Lens' GenesisBlock GenesisBodyAttributes
genBlockAttributes = gbExtra . gebAttributes

instance HasDifficulty GenesisBlock where
    difficultyL = gbHeader . difficultyL

instance HasEpochIndex GenesisBlock where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch


----------------------------------------------------------------------------
-- MainBlock lenses
----------------------------------------------------------------------------

-- | Lens from 'MainBlock' to 'HeaderHash' of its parent.
mainBlockPrevBlock :: Lens' MainBlock HeaderHash
mainBlockPrevBlock = gbPrevBlock

-- | Lens from 'MainBlock' to 'MainProof'.
mainBlockProof :: Lens' MainBlock MainProof
mainBlockProof = gbHeader . mainHeaderProof

-- | Lens from 'MainBlock' to 'SlotId'.
mainBlockSlot :: Lens' MainBlock SlotId
mainBlockSlot = gbHeader . mainHeaderSlot

-- | Lens from 'MainBlock' to 'PublicKey'.
mainBlockLeaderKey :: Lens' MainBlock PublicKey
mainBlockLeaderKey = gbHeader . mainHeaderLeaderKey

-- | Lens from 'MainBlock' to 'ChainDifficulty'.
mainBlockDifficulty :: Lens' MainBlock ChainDifficulty
mainBlockDifficulty = gbHeader . mainHeaderDifficulty

-- | Lens from 'MainBlock' to 'Signature'.
mainBlockSignature :: Lens' MainBlock BlockSignature
mainBlockSignature = gbHeader . mainHeaderSignature

-- | Lens from 'MainBlock' to 'BlockVersion'.
mainBlockBlockVersion :: Lens' MainBlock BlockVersion
mainBlockBlockVersion = gbHeader . mainHeaderBlockVersion

-- | Lens from 'MainBlock' to 'SoftwareVersion'.
mainBlockSoftwareVersion :: Lens' MainBlock SoftwareVersion
mainBlockSoftwareVersion = gbHeader . mainHeaderSoftwareVersion

-- | Lens from 'MainBlock' to 'BlockHeaderAttributes'.
mainBlockHeaderAttributes :: Lens' MainBlock BlockHeaderAttributes
mainBlockHeaderAttributes = gbHeader . mainHeaderAttributes

-- | Lens from 'MainBlock' to proof (hash) of 'MainExtraBodyData'.
mainBlockEBDataProof :: Lens' MainBlock (Hash MainExtraBodyData)
mainBlockEBDataProof = gbHeader . mainHeaderEBDataProof

-- | Lens from 'MainBlock' to 'TxPayload'.
mainBlockTxPayload :: Lens' MainBlock TxPayload
mainBlockTxPayload = gbBody . mbTxPayload

-- | Lens from 'MainBlock' to 'SscPayload'.
mainBlockSscPayload :: Lens' MainBlock SscPayload
mainBlockSscPayload = gbBody . mbSscPayload

-- | Lens from 'MainBlock' to 'UpdatePayload'.
mainBlockUpdatePayload :: Lens' MainBlock UpdatePayload
mainBlockUpdatePayload = gbBody . mbUpdatePayload

-- | Lens from 'MainBlock' to 'DlgPayload'.
mainBlockDlgPayload :: Lens' MainBlock DlgPayload
mainBlockDlgPayload = gbBody . mbDlgPayload

-- | Lens from 'MainBlock' to 'BlockBodyAttributes'.
mainBlockAttributes :: Lens' MainBlock BlockBodyAttributes
mainBlockAttributes = gbExtra . mebAttributes

instance Buildable MainBlock where
    build mainBlock =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions ("%int%" items): "%listJson%"\n"%
             "  "%build%"\n"%
             "  "%build%"\n"%
             "  update payload: "%build%"\n"%
             "  "%build
            )
            (colorize Magenta "MainBlock")
            (mainBlock ^. gbHeader)
            (length txs)
            txs
            (mainBlock ^. mainBlockDlgPayload)
            (mainBlock ^. mainBlockSscPayload)
            (mainBlock ^. mainBlockSscPayload)
            (mainBlock ^. gbExtra)
      where
        txs = mainBlock ^. gbBody . mbTxs

instance HasDifficulty MainBlock where
    difficultyL = mainBlockDifficulty

instance HasEpochIndex MainBlock where
    epochIndexL = mainBlockSlot . epochIndexL

instance HasBlockVersion MainBlock where
    blockVersionL = mainBlockBlockVersion

instance HasSoftwareVersion MainBlock where
    softwareVersionL = mainBlockSoftwareVersion
