{-# LANGUAGE DeriveLift           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains all basic types for @cardano-sl@ update system.

module Pos.Update.Core.Types
       (
         -- * UpdateProposal and related
         UpdateProposal (..)
       , UpId
       , UpAttributes
       , UpdateData (..)
       , BlockVersionData (..)
       , SystemTag (getSystemTag)
       , mkSystemTag
       , systemTagMaxLength
       , patakUpdateData
       , skovorodaUpdateData
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

       -- * VoteState
       , VoteState (..)
       , canCombineVotes
       , combineVotes
       , isPositiveVote
       , newVoteState
       ) where

import           Data.Char                  (isAscii)
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Text.Buildable        as Buildable
import           Data.Text.Lazy.Builder     (Builder)
import           Data.Time.Units            (Millisecond)
import           Formatting                 (Format, bprint, build, builder, int, later,
                                             (%))
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util.Text         (listJson)
import           Universum                  hiding (show)

import           Pos.Binary.Class           (Bi, Raw)
import           Pos.Binary.Crypto          ()
import           Pos.Core.Address           (addressHash)
import           Pos.Core.Coin              ()
import           Pos.Core.Script            ()
import           Pos.Core.Types             (BlockVersion, CoinPortion, FlatSlotId,
                                             ScriptVersion, SoftwareVersion)
import           Pos.Core.Types             ()
import           Pos.Core.Version           ()
import           Pos.Crypto                 (Hash, PublicKey, Signature, hash, shortHashF,
                                             unsafeHash)
import           Pos.Data.Attributes        (Attributes)

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

-- | Proposal for software update
data UpdateProposal = UpdateProposal
    { upBlockVersion     :: !BlockVersion
    , upBlockVersionData :: !BlockVersionData
    , upSoftwareVersion  :: !SoftwareVersion
    , upData             :: !(HM.HashMap SystemTag UpdateData)
    -- ^ UpdateData for each system which this update affects.
    -- It must be non-empty.
    , upAttributes       :: !UpAttributes
    -- ^ Attributes which are currently empty, but provide
    -- extensibility.
    } deriving (Eq, Show, Generic, Typeable)


instance Bi UpdateProposal => Buildable UpdateProposal where
    build up@UpdateProposal {..} =
      bprint (build%
              " { block v"%build%
              ", UpId: "%build%
              ", "%build%
              ", tags: "%listJson%
              ", no attributes "%
              " }")
        upSoftwareVersion
        upBlockVersion
        (hash up)
        upBlockVersionData
        (HM.keys upData)

instance (Bi UpdateProposal) =>
         Buildable (UpdateProposal, [UpdateVote]) where
    build (up, votes) =
        bprint
            (build % " with votes: " %listJson)
            up
            (map formatVoteShort votes)

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
    , bvdUpdateSoftforkThd :: !CoinPortion
    } deriving (Show, Eq, Generic, Typeable)

instance Buildable BlockVersionData where
    build BlockVersionData {..} =
      bprint ("{ scripts v"%build%
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
              ", update softfork threshold: "%build%
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
        bvdUpdateSoftforkThd

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


patakUpdateData :: HM.HashMap SystemTag UpdateData
patakUpdateData =
    let b = "linux64"
        h = unsafeHash b
    in  HM.fromList [(SystemTag b, UpdateData h h h h)]

skovorodaUpdateData :: Hash Raw -> HM.HashMap SystemTag UpdateData
skovorodaUpdateData h =
    let b = "linux64"
    in  HM.fromList [(SystemTag b, UpdateData h h h h)]

instance NFData SystemTag
instance NFData UpdateProposal
-- Proper NFData Millisecond instance should be defined in
-- time-units. I'm sorry for this. volhovm.
instance NFData BlockVersionData where
    rnf BlockVersionData{..} =
        deepseq bvdScriptVersion $
        deepseq (toInteger bvdSlotDuration) $
        deepseq bvdMaxBlockSize $
        deepseq bvdMaxTxSize $
        deepseq bvdMpcThd $
        deepseq bvdHeavyDelThd $
        deepseq bvdUpdateVoteThd $
        deepseq bvdUpdateProposalThd $
        deepseq bvdUpdateImplicit $
        deepseq bvdUpdateSoftforkThd $ ()
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
    --build x = bprint $ show x
    build PositiveVote   = bprint "PositiveVote"
    build NegativeVote   = bprint "NegativeVote"
    build PositiveRevote = bprint "PositiveRevote"
    build NegativeRevote = bprint "NegativeRevote"

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
