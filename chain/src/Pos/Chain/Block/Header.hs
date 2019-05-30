{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- for the Getter instances

module Pos.Chain.Block.Header
       ( BlockHeader (..)
       , blockHeaderProtocolMagicId
       , blockHeaderHash
       , choosingBlockHeader
       , _BlockHeaderGenesis
       , _BlockHeaderMain
       , verifyBlockHeader
       , headerLeaderKey
       , headerLastSlotInfo

       , HeaderHash
       , headerHashF
       , headerHashG

       , HasHeaderHash (..)

       , BlockSignature (..)

       , GenericBlockHeader
       , mkGenericBlockHeaderUnsafe
       , gbhProtocolMagicId
       , gbhPrevBlock
       , gbhBodyProof
       , gbhConsensus
       , gbhExtra

       , GenesisBlockHeader
       , mkGenesisHeader
       , genHeaderPrevBlock
       , genHeaderProof
       , genHeaderEpoch
       , genHeaderDifficulty
       , genHeaderAttributes

       , MainBlockHeader
       , mkMainHeader
       , mkMainHeaderExplicit
       , mainHeaderPrevBlock
       , mainHeaderProof
       , mainHeaderSlot
       , mainHeaderLeaderKey
       , mainHeaderDifficulty
       , mainHeaderSignature
       , mainHeaderBlockVersion
       , mainHeaderSoftwareVersion
       , mainHeaderAttributes
       , mainHeaderEBDataProof
       , verifyMainBlockHeader

       , MainToSign (..)
       , msHeaderHash
       , msBodyProof
       , msSlot
       , msChainDiff
       , msExtraHeader

       , MainConsensusData (..)
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature
       , verifyMainConsensusData
       ) where

import           Universum

import           Codec.CBOR.Decoding (decodeWordCanonical)
import           Codec.CBOR.Encoding (encodeWord)
import           Control.Lens (Getter, LensLike', makeLenses, makePrisms, to)
import           Control.Monad.Except (MonadError (throwError))
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import qualified Data.Serialize as Cereal
import           Formatting (Format, bprint, build, int, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Bi (..), decodeListLenCanonicalOf,
                     encodeListLen, enforceSize)
import           Pos.Chain.Block.Genesis (GenesisBody,
                     GenesisConsensusData (..), GenesisExtraHeaderData (..),
                     GenesisHeaderAttributes, GenesisProof (..), gcdDifficulty,
                     gcdEpoch, gehAttributes, mkGenesisProof)
import           Pos.Chain.Block.Main (BlockHeaderAttributes, MainBody,
                     MainExtraBodyData, MainExtraHeaderData, MainProof (..),
                     mehAttributes, mehBlockVersion, mehEBDataProof,
                     mehSoftwareVersion, mkMainProof,
                     verifyMainExtraHeaderData)
import           Pos.Chain.Delegation.HeavyDlgIndex (ProxySKBlockInfo,
                     ProxySigHeavy)
import           Pos.Chain.Delegation.LightDlgIndices (LightDlgIndices (..),
                     ProxySigLight)
import           Pos.Chain.Genesis.Hash (GenesisHash (..))
import           Pos.Chain.Update.BlockVersion (BlockVersion,
                     HasBlockVersion (..))
import           Pos.Chain.Update.SoftwareVersion (HasSoftwareVersion (..),
                     SoftwareVersion)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.Common (ChainDifficulty, HasDifficulty (..), addressHash)
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..),
                     HasEpochIndex (..), HasEpochOrSlot (..), SlotCount (..),
                     SlotId (..), flattenSlotId, slotIdF)
import           Pos.Crypto (Hash, ProtocolMagic (..), ProtocolMagicId (..),
                     PublicKey, SecretKey, SignTag (..), Signature, checkSig,
                     hashHexF, isSelfSignedPsk, proxySign, proxyVerify,
                     psigPsk, sign, toPublic, unsafeHash)
import           Pos.Util.Some (Some, applySome)
import           Pos.Util.Util (cborError, cerealError)

import           Pos.Chain.Block.Slog.Types (LastSlotInfo (..))

--------------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
--------------------------------------------------------------------------------

-- | Either header of ordinary main block or genesis block.
data BlockHeader
    = BlockHeaderGenesis GenesisBlockHeader
    | BlockHeaderMain MainBlockHeader
    deriving (Eq, Show, Generic, NFData)

instance Buildable BlockHeader where
    build = \case
        BlockHeaderGenesis bhg -> Buildable.build bhg
        BlockHeaderMain    bhm -> Buildable.build bhm

instance HasDifficulty BlockHeader where
    difficultyL = choosingBlockHeader difficultyL difficultyL

instance HasEpochIndex BlockHeader where
    epochIndexL = choosingBlockHeader epochIndexL epochIndexL

instance HasEpochOrSlot BlockHeader where
    getEpochOrSlot = view (choosingBlockHeader (to getEpochOrSlot) (to getEpochOrSlot))

choosingBlockHeader
    :: Functor f
    => LensLike' f GenesisBlockHeader r
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

-- | The 'ProtocolMagic' in a 'BlockHeader'.
blockHeaderProtocolMagicId :: BlockHeader -> ProtocolMagicId
blockHeaderProtocolMagicId (BlockHeaderGenesis gbh) = _gbhProtocolMagicId gbh
blockHeaderProtocolMagicId (BlockHeaderMain mbh)    = _gbhProtocolMagicId mbh

-- | Verify a BlockHeader in isolation. There is nothing to be done for
-- genesis headers.
verifyBlockHeader
    :: MonadError Text m
    => ProtocolMagic
    -> BlockHeader
    -> m ()
verifyBlockHeader _ (BlockHeaderGenesis _) = pure ()
verifyBlockHeader pm (BlockHeaderMain bhm) = verifyMainBlockHeader pm bhm

headerLastSlotInfo :: SlotCount -> BlockHeader -> Maybe LastSlotInfo
headerLastSlotInfo slotCount = \case
    BlockHeaderGenesis _ -> Nothing
    BlockHeaderMain mbh -> Just $ convert mbh
  where
    convert :: MainBlockHeader -> LastSlotInfo
    convert bh =
        LastSlotInfo
            (flattenSlotId slotCount . _mcdSlot $ _gbhConsensus bh)
            (addressHash . _mcdLeaderKey $ _gbhConsensus bh)

--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

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

instance HasHeaderHash BlockHeader where
    headerHash = blockHeaderHash

instance HasHeaderHash GenesisBlockHeader where
    headerHash = blockHeaderHash . BlockHeaderGenesis

instance HasHeaderHash MainBlockHeader where
    headerHash = blockHeaderHash . BlockHeaderMain

headerHashG :: HasHeaderHash a => Getter a HeaderHash
headerHashG = to headerHash

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
--
-- Perhaps, it shouldn't be here, but I decided not to create a module
-- for only this function.
blockHeaderHash :: BlockHeader -> HeaderHash
blockHeaderHash = unsafeHash


--------------------------------------------------------------------------------
-- GenericBlockHeader
--------------------------------------------------------------------------------

-- | Header of block contains some kind of summary. There are various
-- benefits which people get by separating header from other data.
--
-- The constructor has `Unsafe' prefix in its name, because there in
-- general there may be some invariants which must hold for the
-- contents of header.
data GenericBlockHeader bodyProof consensus extra = GenericBlockHeader
    { _gbhProtocolMagicId :: !ProtocolMagicId
      -- | Pointer to the header of the previous block.
    , _gbhPrevBlock       :: !HeaderHash
    , -- | Proof of body.
      _gbhBodyProof       :: !bodyProof
    , -- | Consensus data to verify consensus algorithm.
      _gbhConsensus       :: !consensus
    , -- | Any extra data.
      _gbhExtra           :: !extra
    } deriving (Eq, Show, Generic, NFData)

instance
    (Bi bodyProof, Bi consensus, Bi extra)
    => Bi (GenericBlockHeader bodyProof consensus extra)
  where
    encode bh =  encodeListLen 5
              <> encode (unProtocolMagicId (_gbhProtocolMagicId bh))
              <> encode (_gbhPrevBlock bh)
              <> encode (_gbhBodyProof bh)
              <> encode (_gbhConsensus bh)
              <> encode (_gbhExtra bh)
    decode = do
        enforceSize "GenericBlockHeader b" 5
        _gbhProtocolMagicId <- ProtocolMagicId <$> decode
        _gbhPrevBlock <- decode
        _gbhBodyProof <- decode
        _gbhConsensus <- decode
        _gbhExtra     <- decode
        pure GenericBlockHeader {..}

instance
    (SafeCopy bodyProof, SafeCopy consensus, SafeCopy extra)
    => SafeCopy (GenericBlockHeader bodyProof consensus extra)
  where
    getCopy =
        contain $
        do _gbhProtocolMagicId <- safeGet
           _gbhPrevBlock <- safeGet
           _gbhBodyProof <- safeGet
           _gbhConsensus <- safeGet
           _gbhExtra <- safeGet
           return $! GenericBlockHeader {..}
    putCopy GenericBlockHeader {..} =
        contain $
        do safePut _gbhProtocolMagicId
           safePut _gbhPrevBlock
           safePut _gbhBodyProof
           safePut _gbhConsensus
           safePut _gbhExtra

-- | Export the @GenericBlockHeader@ constructor as an unsafe function for tests
mkGenericBlockHeaderUnsafe
    :: ProtocolMagic
    -> HeaderHash
    -> bodyProof
    -> consensus
    -> extra
    -> GenericBlockHeader bodyProof consensus extra
mkGenericBlockHeaderUnsafe pm = GenericBlockHeader pmi
  where
    pmi = getProtocolMagicId pm


--------------------------------------------------------------------------------
-- GenesisBlockHeader
--------------------------------------------------------------------------------

-- | Header of Genesis block.
type GenesisBlockHeader = GenericBlockHeader
    GenesisProof
    GenesisConsensusData
    GenesisExtraHeaderData

instance Buildable GenesisBlockHeader where
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
        gbhHeaderHash = blockHeaderHash $ BlockHeaderGenesis gbh
        GenesisConsensusData {..} = _gbhConsensus

instance HasEpochOrSlot GenesisBlockHeader where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

-- | Smart constructor for 'GenesisBlockHeader'
mkGenesisHeader
    :: ProtocolMagic
    -> Either GenesisHash BlockHeader
    -> EpochIndex
    -> GenesisBody
    -> GenesisBlockHeader
mkGenesisHeader pm prevHeader epoch body =
    GenericBlockHeader
        pmi
        (either getGenesisHash headerHash prevHeader)
        (mkGenesisProof body)
        consensus
        (GenesisExtraHeaderData $ mkAttributes ())
  where
    pmi = getProtocolMagicId pm
    difficulty = either (const 0) (view difficultyL) prevHeader
    consensus = GenesisConsensusData {_gcdEpoch = epoch, _gcdDifficulty = difficulty}


--------------------------------------------------------------------------------
-- MainBlockHeader
--------------------------------------------------------------------------------

-- | Header of generic main block.
type MainBlockHeader =
    GenericBlockHeader MainProof MainConsensusData MainExtraHeaderData

instance Buildable MainBlockHeader where
    build gbh@GenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    difficulty: "%int%"\n"%
             "    leader: "%build%"\n"%
             "    signature: "%build%"\n"%
             build
            )
            gbhHeaderHash
            _gbhPrevBlock
            _mcdSlot
            _mcdDifficulty
            _mcdLeaderKey
            _mcdSignature
            _gbhExtra
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ BlockHeaderMain gbh
        MainConsensusData {..} = _gbhConsensus

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: ProtocolMagic
    -> Either GenesisHash BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainExtraHeaderData
    -> MainBlockHeader
mkMainHeader pm prevHeader =
    mkMainHeaderExplicit pm prevHash difficulty
  where
    prevHash = either getGenesisHash headerHash prevHeader
    difficulty = either (const 0) (succ . view difficultyL) prevHeader

-- | Make a 'MainBlockHeader' for a given slot, with a given body, parent hash,
-- and difficulty. This takes care of some signing and consensus data.
mkMainHeaderExplicit
    :: ProtocolMagic
    -> HeaderHash -- ^ Parent
    -> ChainDifficulty
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainExtraHeaderData
    -> MainBlockHeader
mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extra =
    GenericBlockHeader pmi prevHash proof consensus extra
  where
    pmi = getProtocolMagicId pm
    proof = mkMainProof body
    makeSignature toSign (psk,_) =
        BlockPSignatureHeavy $ proxySign pm SignMainBlockHeavy sk psk toSign
    signature =
        let toSign = MainToSign prevHash proof slotId difficulty extra
        in maybe
               (BlockSignature $ sign pm SignMainBlock sk toSign)
               (makeSignature toSign)
               pske
    leaderPk = maybe (toPublic sk) snd pske
    consensus =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = leaderPk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature
        }

-- | Verify a main block header in isolation.
verifyMainBlockHeader
    :: MonadError Text m
    => ProtocolMagic
    -> MainBlockHeader
    -> m ()
verifyMainBlockHeader pm GenericBlockHeader {..} = do
    -- Previous header hash is always valid.
    -- Body proof is just a bunch of hashes, which is always valid (although
    -- must be checked against the actual body, in verifyMainBlock.
    -- Consensus data and extra header data require validation.
    verifyMainConsensusData _gbhConsensus
    verifyMainExtraHeaderData _gbhExtra
    -- Internal consistency: is the signature in the consensus data really for
    -- this block?
    unless (verifyBlockSignature _mcdSignature) $
        throwError "can't verify signature"

  where

    verifyBlockSignature (BlockSignature sig) =
        checkSig pm SignMainBlock leaderPk signature sig
    verifyBlockSignature (BlockPSignatureLight proxySig) =
        proxyVerify
            pm
            SignMainBlockLight
            proxySig
            (\(LightDlgIndices (epochLow, epochHigh)) ->
                 epochLow <= epochId && epochId <= epochHigh)
            signature
    verifyBlockSignature (BlockPSignatureHeavy proxySig) =
        proxyVerify pm SignMainBlockHeavy proxySig (const True) signature
    signature = MainToSign _gbhPrevBlock _gbhBodyProof slotId difficulty _gbhExtra
    epochId = siEpoch slotId
    MainConsensusData
        { _mcdLeaderKey = leaderPk
        , _mcdSlot = slotId
        , _mcdDifficulty = difficulty
        , ..
        } = _gbhConsensus


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

-- | Signature of the block. Can be either regular signature from the
-- issuer or delegated signature having a constraint on epoch indices
-- (it means the signature is valid only if block's slot id has epoch
-- inside the constrained interval).
data BlockSignature
    = BlockSignature (Signature MainToSign)
    | BlockPSignatureLight (ProxySigLight MainToSign)
    | BlockPSignatureHeavy (ProxySigHeavy MainToSign)
    deriving (Show, Eq, Generic)

instance NFData BlockSignature

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

instance SafeCopy BlockSignature where
    getCopy = contain $ Cereal.getWord8 >>= \case
        0 -> BlockSignature <$> safeGet
        1 -> BlockPSignatureLight <$> safeGet
        2 -> BlockPSignatureHeavy <$> safeGet
        t -> cerealError $ "getCopy@BlockSignature: couldn't read tag: " <> show t
    putCopy (BlockSignature sig)            = contain $ Cereal.putWord8 0 >> safePut sig
    putCopy (BlockPSignatureLight proxySig) = contain $ Cereal.putWord8 1 >> safePut proxySig
    putCopy (BlockPSignatureHeavy proxySig) = contain $ Cereal.putWord8 2 >> safePut proxySig

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


--------------------------------------------------------------------------------
-- MainConsensusData
--------------------------------------------------------------------------------

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

instance SafeCopy MainConsensusData where
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

-- | Verify the consensus data in isolation.
verifyMainConsensusData
    :: ( MonadError Text m )
    => MainConsensusData
    -> m ()
verifyMainConsensusData MainConsensusData {..} = do
    when (selfSignedProxy _mcdSignature) $
        throwError "can't use self-signed psk to issue the block"
  where
    selfSignedProxy (BlockSignature _)                      = False
    selfSignedProxy (BlockPSignatureLight (psigPsk -> psk)) = isSelfSignedPsk psk
    selfSignedProxy (BlockPSignatureHeavy (psigPsk -> psk)) = isSelfSignedPsk psk


--------------------------------------------------------------------------------
-- BlockHeaderGenesis prisms
--------------------------------------------------------------------------------

makePrisms 'BlockHeaderGenesis


--------------------------------------------------------------------------------
-- GenericBlockHeader Lenses
--------------------------------------------------------------------------------

makeLenses ''GenericBlockHeader

instance HasDifficulty GenesisBlockHeader where
    difficultyL = gbhConsensus . difficultyL

instance HasEpochIndex GenesisBlockHeader where
    epochIndexL = gbhConsensus . gcdEpoch


--------------------------------------------------------------------------------
-- MainConsensusData lenses
--------------------------------------------------------------------------------

makeLenses 'MainConsensusData

instance HasDifficulty MainConsensusData where
    difficultyL = mcdDifficulty


----------------------------------------------------------------------------
-- GenesisBlockHeader lenses
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlockHeader' to 'HeaderHash' of its parent.
genHeaderPrevBlock :: Lens' GenesisBlockHeader HeaderHash
genHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'GenesisBlockHeader' to 'GenesisProof'.
genHeaderProof :: Lens' GenesisBlockHeader GenesisProof
genHeaderProof = gbhBodyProof

-- | Lens from 'GenesisBlockHeader' to 'EpochIndex'.
genHeaderEpoch :: Lens' GenesisBlockHeader EpochIndex
genHeaderEpoch = gbhConsensus . gcdEpoch

-- | Lens from 'GenesisBlockHeader' to 'ChainDifficulty'.
genHeaderDifficulty :: Lens' GenesisBlockHeader ChainDifficulty
genHeaderDifficulty = gbhConsensus . gcdDifficulty

-- | Lens from 'GenesisBlockHeader' to 'GenesisHeaderAttributes'.
genHeaderAttributes :: Lens' GenesisBlockHeader GenesisHeaderAttributes
genHeaderAttributes = gbhExtra . gehAttributes


--------------------------------------------------------------------------------
-- MainBlockHeader lenses
--------------------------------------------------------------------------------

-- | Lens from 'MainBlockHeader' to 'HeaderHash' of its parent.
mainHeaderPrevBlock :: Lens' MainBlockHeader HeaderHash
mainHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'MainBlockHeader' to 'MainProof'.
mainHeaderProof :: Lens' MainBlockHeader MainProof
mainHeaderProof = gbhBodyProof

-- | Lens from 'MainBlockHeader' to 'SlotId'.
mainHeaderSlot :: Lens' MainBlockHeader SlotId
mainHeaderSlot = gbhConsensus . mcdSlot

-- | Lens from 'MainBlockHeader' to 'PublicKey'.
mainHeaderLeaderKey :: Lens' MainBlockHeader PublicKey
mainHeaderLeaderKey = gbhConsensus . mcdLeaderKey

headerLeaderKey :: BlockHeader -> Maybe PublicKey
headerLeaderKey = \case
    BlockHeaderGenesis _ -> Nothing
    BlockHeaderMain mbh -> Just $ view mainHeaderLeaderKey mbh

-- | Lens from 'MainBlockHeader' to 'ChainDifficulty'.
mainHeaderDifficulty :: Lens' MainBlockHeader ChainDifficulty
mainHeaderDifficulty = gbhConsensus . mcdDifficulty

-- | Lens from 'MainBlockHeader' to 'Signature'.
mainHeaderSignature :: Lens' MainBlockHeader BlockSignature
mainHeaderSignature = gbhConsensus . mcdSignature

-- | Lens from 'MainBlockHeader' to 'BlockVersion'.
mainHeaderBlockVersion :: Lens' MainBlockHeader BlockVersion
mainHeaderBlockVersion = gbhExtra . mehBlockVersion

-- | Lens from 'MainBlockHeader' to 'SoftwareVersion'.
mainHeaderSoftwareVersion :: Lens' MainBlockHeader SoftwareVersion
mainHeaderSoftwareVersion = gbhExtra . mehSoftwareVersion

-- | Lens from 'MainBlockHeader' to 'BlockHeaderAttributes'.
mainHeaderAttributes :: Lens' MainBlockHeader BlockHeaderAttributes
mainHeaderAttributes = gbhExtra . mehAttributes

-- | Lens from 'MainBlockHeader' to 'MainExtraBodyData'
mainHeaderEBDataProof :: Lens' MainBlockHeader (Hash MainExtraBodyData)
mainHeaderEBDataProof = gbhExtra . mehEBDataProof

instance HasDifficulty MainBlockHeader where
    difficultyL = mainHeaderDifficulty

instance HasEpochOrSlot MainBlockHeader where
    getEpochOrSlot = EpochOrSlot . Right . view mainHeaderSlot

instance HasEpochIndex MainBlockHeader where
    epochIndexL = mainHeaderSlot . epochIndexL

instance HasBlockVersion MainBlockHeader where
    blockVersionL = mainHeaderBlockVersion

instance HasSoftwareVersion MainBlockHeader where
    softwareVersionL = mainHeaderSoftwareVersion


----------------------------------------------------------------------------
-- MainToSign lenses
----------------------------------------------------------------------------

makeLenses ''MainToSign
