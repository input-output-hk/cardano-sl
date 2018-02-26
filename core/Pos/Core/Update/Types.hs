{-# LANGUAGE DeriveLift #-}

-- | Core types related to update system.

module Pos.Core.Update.Types
       (
         -- * Version
         ApplicationName (..)
       , applicationNameMaxLength
       , checkApplicationName
       , BlockVersion (..)
       , NumSoftwareVersion
       , SoftwareVersion (..)
       , checkSoftwareVersion

       -- * Data associated with block version
       , SoftforkRule (..)
       , BlockVersionData (..)
       , BlockVersionModifier (..)

         -- * UpdateProposal and related
       , UpdateProposal (..)
       , UpdateProposals
       , UpId
       , UpAttributes
       , UpdateData (..)
       , UpdateProposalToSign (..)
       , SystemTag (..)
       , checkSystemTag
       , systemTagMaxLength

         -- * UpdateVote and related
       , UpdateVote (..)
       , mkUpdateVote
       , mkUpdateVoteSafe
       , VoteId
       , formatVoteShort
       , shortVoteF

         -- * Payload and proof
       , UpdatePayload (..)
       , UpdateProof
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Char (isAscii)
import           Data.Default (Default (..))
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Buildable as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Data.Time.Units (Millisecond)
import           Formatting (Format, bprint, build, builder, int, later, shown, stext, (%))
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Syntax (Lift)
import qualified Prelude
import           Serokell.AcidState ()
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (Bi, Raw)
import           Pos.Core.Common (CoinPortion, ScriptVersion, TxFeePolicy, addressHash)
import           Pos.Core.Slotting.Types (EpochIndex, FlatSlotId)
import           Pos.Crypto (ProtocolMagic, Hash, PublicKey, SafeSigner, SecretKey,
                             SignTag (SignUSVote), Signature, hash, safeSign,
                             safeToPublic, shortHashF, sign, toPublic)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)
import           Pos.Util.Orphans ()

----------------------------------------------------------------------------
-- Version
----------------------------------------------------------------------------

-- | Communication protocol version.
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvMinor :: !Word16
    , bvAlt   :: !Word8
    } deriving (Eq, Generic, Ord, Typeable)

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable, NFData)

-- | Smart constructor of 'ApplicationName'.
checkApplicationName :: MonadError Text m => ApplicationName -> m ()
checkApplicationName (ApplicationName appName)
    | length appName > applicationNameMaxLength =
        throwError "ApplicationName: too long string passed"
    | T.any (not . isAscii) appName =
        throwError "ApplicationName: not ascii string passed"
    | otherwise = pure ()

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 12

-- | Numeric software version associated with ApplicationName.
type NumSoftwareVersion = Word32

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    } deriving (Eq, Generic, Ord, Typeable)

instance Buildable SoftwareVersion where
    build SoftwareVersion {..} =
        bprint (stext % ":" % int) (getApplicationName svAppName) svNumber

instance Show SoftwareVersion where
    show = toString . pretty

instance Show BlockVersion where
    show BlockVersion {..} =
        intercalate "." [show bvMajor, show bvMinor, show bvAlt]

instance Buildable BlockVersion where
    build = bprint shown

instance Hashable SoftwareVersion
instance Hashable BlockVersion

instance NFData BlockVersion
instance NFData SoftwareVersion

-- | A software version is valid iff its application name is valid.
checkSoftwareVersion :: MonadError Text m => SoftwareVersion -> m ()
checkSoftwareVersion sv = checkApplicationName (svAppName sv)

----------------------------------------------------------------------------
-- Values updatable by update system
----------------------------------------------------------------------------

-- | Values defining softfork resolution rule.
-- If a proposal is confirmed at the 's'-th epoch, softfork resolution threshold
-- at the 't'-th epoch will be
-- 'max spMinThd (spInitThd - (t - s) * spThdDecrement)'.
--
-- Softfork resolution threshold is the portion of total stake such
-- that if total stake of issuers of blocks with some block version is
-- greater than this portion, this block version becomes adopted.
data SoftforkRule = SoftforkRule
    { srInitThd      :: !CoinPortion
    -- ^ Initial threshold (right after proposal is confirmed).
    , srMinThd       :: !CoinPortion
    -- ^ Minimal threshold (i. e. threshold can't become less than
    -- this one).
    , srThdDecrement :: !CoinPortion
    -- ^ Theshold will be decreased by this value after each epoch.
    } deriving (Show, Eq, Ord, Generic)

instance Hashable SoftforkRule
instance NFData SoftforkRule

instance Buildable SoftforkRule where
    build SoftforkRule {..} =
        bprint ("(init = "%build%", min = "%build%", decrement = "%build%")")
        srInitThd srMinThd srThdDecrement

-- | Data which is associated with 'BlockVersion'.
data BlockVersionData = BlockVersionData
    { bvdScriptVersion     :: !ScriptVersion
    , bvdSlotDuration      :: !Millisecond
    , bvdMaxBlockSize      :: !Byte
    , bvdMaxHeaderSize     :: !Byte
    , bvdMaxTxSize         :: !Byte
    , bvdMaxProposalSize   :: !Byte
    , bvdMpcThd            :: !CoinPortion
    , bvdHeavyDelThd       :: !CoinPortion
    , bvdUpdateVoteThd     :: !CoinPortion
    , bvdUpdateProposalThd :: !CoinPortion
    , bvdUpdateImplicit    :: !FlatSlotId
    , bvdSoftforkRule      :: !SoftforkRule
    , bvdTxFeePolicy       :: !TxFeePolicy
    , bvdUnlockStakeEpoch  :: !EpochIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData BlockVersionData where

instance Buildable BlockVersionData where
    build BlockVersionData {..} =
      bprint ("{ script version: "%build%
              ", slot duration: "%int%" mcs"%
              ", block size limit: "%memory%
              ", header size limit: "%memory%
              ", tx size limit: "%memory%
              ", proposal size limit: "%memory%
              ", mpc threshold: "%build%
              ", heavyweight delegation threshold: "%build%
              ", update vote threshold: "%build%
              ", update proposal threshold: "%build%
              ", update implicit period: "%int%" slots"%
              ", softfork rule: "%build%
              ", tx fee policy: "%build%
              ", unlock stake epoch: "%build%
              " }")
        bvdScriptVersion
        bvdSlotDuration
        bvdMaxBlockSize
        bvdMaxHeaderSize
        bvdMaxTxSize
        bvdMaxProposalSize
        bvdMpcThd
        bvdHeavyDelThd
        bvdUpdateVoteThd
        bvdUpdateProposalThd
        bvdUpdateImplicit
        bvdSoftforkRule
        bvdTxFeePolicy
        bvdUnlockStakeEpoch

-- | Data which represents modifications of block (aka protocol) version.
data BlockVersionModifier = BlockVersionModifier
    { bvmScriptVersion     :: !(Maybe ScriptVersion)
    , bvmSlotDuration      :: !(Maybe Millisecond)
    , bvmMaxBlockSize      :: !(Maybe Byte)
    , bvmMaxHeaderSize     :: !(Maybe Byte)
    , bvmMaxTxSize         :: !(Maybe Byte)
    , bvmMaxProposalSize   :: !(Maybe Byte)
    , bvmMpcThd            :: !(Maybe CoinPortion)
    , bvmHeavyDelThd       :: !(Maybe CoinPortion)
    , bvmUpdateVoteThd     :: !(Maybe CoinPortion)
    , bvmUpdateProposalThd :: !(Maybe CoinPortion)
    , bvmUpdateImplicit    :: !(Maybe FlatSlotId)
    , bvmSoftforkRule      :: !(Maybe SoftforkRule)
    , bvmTxFeePolicy       :: !(Maybe TxFeePolicy)
    , bvmUnlockStakeEpoch  :: !(Maybe EpochIndex)
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData BlockVersionModifier
instance Hashable BlockVersionModifier

instance Default BlockVersionModifier where
    def = BlockVersionModifier
        { bvmScriptVersion     = Nothing
        , bvmSlotDuration      = Nothing
        , bvmMaxBlockSize      = Nothing
        , bvmMaxHeaderSize     = Nothing
        , bvmMaxTxSize         = Nothing
        , bvmMaxProposalSize   = Nothing
        , bvmMpcThd            = Nothing
        , bvmHeavyDelThd       = Nothing
        , bvmUpdateVoteThd     = Nothing
        , bvmUpdateProposalThd = Nothing
        , bvmUpdateImplicit    = Nothing
        , bvmSoftforkRule      = Nothing
        , bvmTxFeePolicy       = Nothing
        , bvmUnlockStakeEpoch  = Nothing
        }

instance Buildable BlockVersionModifier where
    build BlockVersionModifier {..} =
      bprint ("{ script version: "%bmodifier build%
              ", slot duration (mcs): "%bmodifier int%
              ", block size limit: "%bmodifier memory%
              ", header size limit: "%bmodifier memory%
              ", tx size limit: "%bmodifier memory%
              ", proposal size limit: "%bmodifier memory%
              ", mpc threshold: "%bmodifier build%
              ", heavyweight delegation threshold: "%bmodifier build%
              ", update vote threshold: "%bmodifier build%
              ", update proposal threshold: "%bmodifier build%
              ", update implicit period (slots): "%bmodifier int%
              ", softfork rule: "%bmodifier build%
              ", tx fee policy: "%bmodifier build%
              ", unlock stake epoch: "%bmodifier build%
              " }")
        bvmScriptVersion
        bvmSlotDuration
        bvmMaxBlockSize
        bvmMaxHeaderSize
        bvmMaxTxSize
        bvmMaxProposalSize
        bvmMpcThd
        bvmHeavyDelThd
        bvmUpdateVoteThd
        bvmUpdateProposalThd
        bvmUpdateImplicit
        bvmSoftforkRule
        bvmTxFeePolicy
        bvmUnlockStakeEpoch
      where
        bmodifier :: Format Builder (a -> Builder) -> Format r (Maybe a -> r)
        bmodifier b = later $ maybe "no change" (bprint b)

----------------------------------------------------------------------------
-- UpdateProposal and related
----------------------------------------------------------------------------

-- | Tag of system for which update data is purposed, e.g. win64, mac32
newtype SystemTag = SystemTag { getSystemTag :: Text }
  deriving (Eq, Ord, Show, Generic, Buildable, Hashable, Lift, Typeable)

systemTagMaxLength :: Integral i => i
systemTagMaxLength = 10

checkSystemTag :: MonadError Text m => SystemTag -> m ()
checkSystemTag (SystemTag tag)
    | T.length tag > systemTagMaxLength
          = throwError "SystemTag: too long string passed"
    | T.any (not . isAscii) tag
          = throwError "SystemTag: not ascii string passed"
    | otherwise
          = pure ()

-- | ID of software update proposal
type UpId = Hash UpdateProposal

type UpAttributes = Attributes ()

data UpdateProposalToSign
    = UpdateProposalToSign
    { upsBV   :: !BlockVersion
    , upsBVM  :: !BlockVersionModifier
    , upsSV   :: !SoftwareVersion
    , upsData :: !(HM.HashMap SystemTag UpdateData)
    , upsAttr :: !UpAttributes
    } deriving (Eq, Show, Generic)

-- | Proposal for software update
data UpdateProposal = UnsafeUpdateProposal
    { upBlockVersion    :: !BlockVersion
    , upBlockVersionMod :: !BlockVersionModifier
    , upSoftwareVersion :: !SoftwareVersion
    , upData            :: !(HM.HashMap SystemTag UpdateData)
    -- ^ UpdateData for each system which this update affects.
    -- It must be non-empty.
    , upAttributes      :: !UpAttributes
    -- ^ Attributes which are currently empty, but provide
    -- extensibility.
    , upFrom            :: !PublicKey
    -- ^ Who proposed this UP.
    , upSignature       :: !(Signature UpdateProposalToSign)
    } deriving (Eq, Show, Generic, Typeable)

type UpdateProposals = HashMap UpId UpdateProposal

instance Hashable UpdateProposal

instance Bi UpdateProposal => Buildable UpdateProposal where
    build up@UnsafeUpdateProposal {..} =
      bprint (build%
              " { block v"%build%
              ", UpId: "%build%
              ", "%build%
              ", tags: "%listJson%
              ", "%builder%
              " }")
        upSoftwareVersion
        upBlockVersion
        (hash up)
        upBlockVersionMod
        (HM.keys upData)
        attrsBuilder
      where
        attrs = upAttributes
        attrsBuilder
            | areAttributesKnown upAttributes = "no attributes"
            | otherwise = bprint ("attributes: " %build) attrs

instance (Bi UpdateProposal) =>
         Buildable (UpdateProposal, [UpdateVote]) where
    build (up, votes) =
        bprint
            (build % " with votes: " %listJson)
            up
            (map formatVoteShort votes)

-- | Data which describes update. It is specific for each system.
data UpdateData = UpdateData
    { udAppDiffHash  :: !(Hash Raw)
    -- ^ Hash of binary diff between two applications. This diff can
    -- be passed to updater to create new application.
    , udPkgHash      :: !(Hash Raw)
    -- ^ Hash of package to install new application. This package can
    -- be used to install new application from scratch instead of
    -- updating existing application.
    , udUpdaterHash  :: !(Hash Raw)
    -- ^ Hash if update application which can be used to install this
    -- update (relevant only when updater is used, not package).
    , udMetadataHash :: !(Hash Raw)
    -- ^ Hash of metadata relevant to this update.  It is raw hash,
    -- because metadata can include image or something
    -- (maybe). Anyway, we can always use `unsafeHash`.
    } deriving (Eq, Show, Generic, Typeable)


instance NFData SystemTag
instance NFData UpdateProposal
-- Proper NFData Millisecond instance should be defined in
-- time-units. I'm sorry for this. volhovm.
instance NFData UpdateData

instance Hashable UpdateData

instance Buildable UpdateData where
    build UpdateData {..} =
      bprint ("{ appDiff: "%build%
              ", pkg: "%build%
              ", updater: "%build%
              ", metadata: "%build%
              " }")
        udAppDiffHash
        udPkgHash
        udUpdaterHash
        udMetadataHash

----------------------------------------------------------------------------
-- UpdateVote and related
----------------------------------------------------------------------------

type VoteId = (UpId, PublicKey, Bool)

-- | Vote for update proposal.
--
-- Invariants:
--   * The signature is valid.
data UpdateVote = UnsafeUpdateVote
    { -- | Public key of stakeholder, who votes
      uvKey        :: !PublicKey
    , -- | Proposal to which this vote applies
      uvProposalId :: !UpId
    , -- | Approval/rejection bit
      uvDecision   :: !Bool
    , -- | Signature of (Update proposal, Approval/rejection bit)
      --   by stakeholder
      uvSignature  :: !(Signature (UpId, Bool))
    } deriving (Eq, Show, Generic, Typeable)

instance NFData UpdateVote

instance Buildable UpdateVote where
    build UnsafeUpdateVote {..} =
      bprint ("Update Vote { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             (addressHash uvKey) uvProposalId uvDecision

instance Buildable VoteId where
    build (upId, pk, dec) =
      bprint ("Vote Id { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             pk upId dec

-- | A safe constructor for 'UnsafeVote'.
mkUpdateVote
    :: ProtocolMagic
    -> SecretKey           -- ^ The voter
    -> UpId                -- ^ Proposal which is voted for
    -> Bool                -- ^ Approval/rejection bit
    -> UpdateVote
mkUpdateVote pm sk uvProposalId uvDecision =
    let uvSignature = sign pm SignUSVote sk (uvProposalId, uvDecision)
        uvKey       = toPublic sk
    in  UnsafeUpdateVote{..}

-- | Same as 'mkUpdateVote', but uses 'SafeSigner'.
mkUpdateVoteSafe
    :: ProtocolMagic
    -> SafeSigner          -- ^ The voter
    -> UpId                -- ^ Proposal which is voted for
    -> Bool                -- ^ Approval/rejection bit
    -> UpdateVote
mkUpdateVoteSafe pm sk uvProposalId uvDecision =
    let uvSignature = safeSign pm SignUSVote sk (uvProposalId, uvDecision)
        uvKey       = safeToPublic sk
    in  UnsafeUpdateVote{..}

-- | Format 'UpdateVote' compactly.
formatVoteShort :: UpdateVote -> Builder
formatVoteShort UnsafeUpdateVote {..} =
    bprint ("("%shortHashF%" "%builder%" "%shortHashF%")")
        (addressHash uvKey)
        (bool "against" "for" uvDecision)
        uvProposalId

-- | Formatter for 'UpdateVote' which displays it compactly.
shortVoteF :: Format r (UpdateVote -> r)
shortVoteF = later formatVoteShort

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

-- | Update System payload. 'BodyProof MainBlockchain' contains
-- 'UpdateProof' = @Hash UpdatePayload@.
data UpdatePayload = UpdatePayload
    { upProposal :: !(Maybe UpdateProposal)
    , upVotes    :: ![UpdateVote]
    } deriving (Eq, Show, Generic, Typeable)

instance NFData UpdatePayload

instance (Bi UpdateProposal) => Buildable UpdatePayload where
    build UpdatePayload {..}
        | null upVotes = formatMaybeProposal upProposal <> ", no votes"
        | otherwise =
            formatMaybeProposal upProposal <>
            bprint
                ("\n    votes: "%listJson)
                (map formatVoteShort upVotes)

formatMaybeProposal :: Bi UpdateProposal => Maybe UpdateProposal -> Builder
formatMaybeProposal = maybe "no proposal" Buildable.build

instance Default UpdatePayload where
    def = UpdatePayload Nothing []

-- | Proof that body of update message contains 'UpdatePayload'.
type UpdateProof = Hash UpdatePayload
