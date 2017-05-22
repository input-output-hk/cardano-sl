{-# LANGUAGE DeriveLift #-}

-- | This module contains all basic types for @cardano-sl@ update system.

module Pos.Update.Core.Types
       (
         -- * UpdateProposal and related
         UpdateProposal (..)
       , UpId
       , UpAttributes
       , UpdateData (..)
       , UpdateProposalToSign (..)
       , BlockVersionData (..)
       , SystemTag (getSystemTag)
       , mkUpdateProposal
       , mkUpdateProposalWSign
       , mkSystemTag
       , systemTagMaxLength
       , upScriptVersion
       , upSlotDuration
       , upMaxBlockSize
       , formatVoteShort
       , shortVoteF

         -- * UpdateVote and related
       , UpdateVote (..)
       , VoteId
       , StakeholderVotes
       , UpdateProposals
       , LocalVotes
       , mkVoteId

         -- * Payload and proof
       , UpdatePayload (..)
       , UpdateProof
       , mkUpdateProof

       -- * Block
       , UpdateBlock

       -- * VoteState
       , VoteState (..)
       , canCombineVotes
       , combineVotes
       , isPositiveVote
       , newVoteState
       ) where

import           Universum

import           Data.Char                  (isAscii)
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Text.Buildable        as Buildable
import           Data.Text.Lazy.Builder     (Builder)
import           Data.Time.Units            (Millisecond)
import           Formatting                 (Format, bprint, build, builder, later, (%))
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Text         (listJson)

import           Pos.Binary.Class           (Bi, Raw)
import           Pos.Binary.Crypto          ()
import           Pos.Core                   (BlockVersion, BlockVersionData (..),
                                             IsGenesisHeader, IsMainHeader, ScriptVersion,
                                             SoftwareVersion, addressHash)
import           Pos.Crypto                 (Hash, PublicKey, SafeSigner,
                                             SignTag (SignUSProposal), Signature,
                                             checkSig, hash, safeSign, safeToPublic,
                                             shortHashF)
import           Pos.Data.Attributes        (Attributes (attrRemain))
import           Pos.Util.Util              (Some)

----------------------------------------------------------------------------
-- UpdateProposal and related
----------------------------------------------------------------------------

-- | Tag of system for which update data is purposed, e.g. win64, mac32
newtype SystemTag = SystemTag { getSystemTag :: Text }
  deriving (Eq, Ord, Show, Generic, Buildable, Hashable, Lift, Typeable)

systemTagMaxLength :: Integral i => i
systemTagMaxLength = 10

mkSystemTag :: MonadFail m => Text -> m SystemTag
mkSystemTag tag | T.length tag > systemTagMaxLength
                    = fail "SystemTag: too long string passed"
                | T.any (not . isAscii) tag
                    = fail "SystemTag: not ascii string passed"
                | otherwise
                    = pure $ SystemTag tag

-- | ID of software update proposal
type UpId = Hash UpdateProposal

type UpAttributes = Attributes ()

data UpdateProposalToSign
    = UpdateProposalToSign
    { upsBV   :: !BlockVersion
    , upsBVD  :: !BlockVersionData
    , upsSV   :: !SoftwareVersion
    , upsData :: !(HM.HashMap SystemTag UpdateData)
    , upsAttr :: !UpAttributes
    }

-- | Proposal for software update
data UpdateProposal = UnsafeUpdateProposal
    { upBlockVersion     :: !BlockVersion
    , upBlockVersionData :: !BlockVersionData
    , upSoftwareVersion  :: !SoftwareVersion
    , upData             :: !(HM.HashMap SystemTag UpdateData)
    -- ^ UpdateData for each system which this update affects.
    -- It must be non-empty.
    , upAttributes       :: !UpAttributes
    -- ^ Attributes which are currently empty, but provide
    -- extensibility.
    , upFrom             :: !PublicKey
    -- ^ Who proposed this UP.
    , upSignature        :: !(Signature UpdateProposalToSign)
    } deriving (Eq, Show, Generic, Typeable)

mkUpdateProposal
    :: (MonadFail m, Bi UpdateProposalToSign)
    => BlockVersion
    -> BlockVersionData
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> PublicKey
    -> Signature UpdateProposalToSign
    -> m UpdateProposal
mkUpdateProposal
    upBlockVersion
    upBlockVersionData
    upSoftwareVersion
    upData
    upAttributes
    upFrom
    upSignature = do
        when (HM.null upData) $ -- Check if proposal data is non-empty
            fail "UpdateProposal: empty proposal data"
        let toSign =
                UpdateProposalToSign
                    upBlockVersion
                    upBlockVersionData
                    upSoftwareVersion
                    upData
                    upAttributes
        unless (checkSig SignUSProposal upFrom toSign upSignature) $
            fail $ "UpdateProposal: signature is invalid"
        pure UnsafeUpdateProposal{..}

mkUpdateProposalWSign
    :: (MonadFail m, Bi UpdateProposalToSign)
    => BlockVersion
    -> BlockVersionData
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> SafeSigner
    -> m UpdateProposal
mkUpdateProposalWSign
    upBlockVersion
    upBlockVersionData
    upSoftwareVersion
    upData
    upAttributes
    ss = do
        when (HM.null upData) $ -- Check if proposal data is non-empty
            fail "UpdateProposal: empty proposal data"
        let toSign =
                UpdateProposalToSign
                    upBlockVersion
                    upBlockVersionData
                    upSoftwareVersion
                    upData
                    upAttributes
        let upFrom = safeToPublic ss
        let upSignature = safeSign SignUSProposal ss toSign
        pure UnsafeUpdateProposal{..}

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
        upBlockVersionData
        (HM.keys upData)
        attrsBuilder
      where
        attrs = upAttributes
        attrsBuilder
            | null (attrRemain upAttributes) = "no attributes"
            | otherwise = bprint ("attributes: " %build) attrs

instance (Bi UpdateProposal) =>
         Buildable (UpdateProposal, [UpdateVote]) where
    build (up, votes) =
        bprint
            (build % " with votes: " %listJson)
            up
            (map formatVoteShort votes)

upScriptVersion :: UpdateProposal -> ScriptVersion
upScriptVersion = bvdScriptVersion . upBlockVersionData

upSlotDuration :: UpdateProposal -> Millisecond
upSlotDuration = bvdSlotDuration . upBlockVersionData

upMaxBlockSize :: UpdateProposal -> Byte
upMaxBlockSize = bvdMaxBlockSize . upBlockVersionData

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

----------------------------------------------------------------------------
-- UpdateVote and related
----------------------------------------------------------------------------

type VoteId = (UpId, PublicKey, Bool)

-- | Vote for update proposal
data UpdateVote = UpdateVote
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
    build UpdateVote {..} =
      bprint ("Update Vote { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             (addressHash uvKey) uvProposalId uvDecision

-- | Format 'UpdateVote' compactly.
formatVoteShort :: UpdateVote -> Builder
formatVoteShort UpdateVote {..} =
    bprint ("("%shortHashF%" "%builder%" "%shortHashF%")")
        (addressHash uvKey)
        (bool "against" "for" uvDecision)
        uvProposalId

-- | Formatter for 'UpdateVote' which displays it compactly.
shortVoteF :: Format r (UpdateVote -> r)
shortVoteF = later formatVoteShort

instance Buildable VoteId where
    build (upId, pk, dec) =
      bprint ("Vote Id { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             pk upId dec

mkVoteId :: UpdateVote -> VoteId
mkVoteId UpdateVote{..} = (uvProposalId, uvKey, uvDecision)

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

-- | Update System payload. 'Pos.Types.BodyProof' contains 'UpdateProof' = @Hash UpdatePayload@.
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

mkUpdateProof
    :: Bi UpdatePayload
    => UpdatePayload -> UpdateProof
mkUpdateProof = hash

----------------------------------------------------------------------------
-- Block
----------------------------------------------------------------------------

-- TODO: I don't like that 'Some' is used here
-- —@neongreen
type UpdateBlock = Either (Some IsGenesisHeader) (Some IsMainHeader, UpdatePayload)

----------------------------------------------------------------------------
-- VoteState
----------------------------------------------------------------------------

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.
    deriving (Show, Generic, Eq)

instance NFData VoteState

instance Buildable VoteState where
    build PositiveVote   = "PositiveVote"
    build NegativeVote   = "NegativeVote"
    build PositiveRevote = "PositiveRevote"
    build NegativeRevote = "NegativeRevote"

-- | Create new VoteState from bool, which is simple vote, not revote.
newVoteState :: Bool -> VoteState
newVoteState True  = PositiveVote
newVoteState False = NegativeVote

isPositiveVote :: VoteState -> Bool
isPositiveVote PositiveVote   = True
isPositiveVote PositiveRevote = True
isPositiveVote _              = False

-- | Check whether given decision is a valid vote if applied to
-- existing vote (which may not exist).
canCombineVotes :: Bool -> Maybe VoteState -> Bool
canCombineVotes _ Nothing                 = True
canCombineVotes True (Just NegativeVote)  = True
canCombineVotes False (Just PositiveVote) = True
canCombineVotes _ _                       = False

-- | Apply decision to given vote (or Nothing). This function returns
-- 'Nothing' if decision can't be applied. 'canCombineVotes' can be
-- used to check whether it will be successful.
combineVotes :: Bool -> Maybe VoteState -> Maybe VoteState
combineVotes decision oldVote =
    case (decision, oldVote) of
        (True, Nothing)            -> Just PositiveVote
        (False, Nothing)           -> Just NegativeVote
        (True, Just NegativeVote)  -> Just PositiveRevote
        (False, Just PositiveVote) -> Just NegativeRevote
        (_, Just _)                -> Nothing

-- | Type alias for set of votes from stakeholders
type StakeholderVotes = HashMap PublicKey VoteState

type UpdateProposals = HashMap UpId UpdateProposal
type LocalVotes = HashMap UpId (HashMap PublicKey UpdateVote)
