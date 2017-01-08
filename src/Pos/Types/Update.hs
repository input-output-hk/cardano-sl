{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Update
       ( UpdateProposal (..)
       , UpId
       , ProposalMsgTag (..)

       , UpdateVote (..)
       , VoteMsgTag (..)
       , StakeholderVotes
       , VoteState (..)

       , UpdateData (..)
       , SystemTag (getSystemTag)
       , mkSystemTag
       , systemTagMaxLength
       , canCombineVotes
       , combineVotes
       ) where

import           Control.Exception          (assert)
import           Data.Char                  (isAscii)
import qualified Data.HashMap.Strict        as HM
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable)
import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, build, sformat, shown, (%))
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Util.Text         (listJson)
import           Universum                  hiding (show)

import           Pos.Crypto                 (Hash, PublicKey, Signature)
import           Pos.Script.Type            (ScriptVersion)
import           Pos.Types.Version          (ProtocolVersion, SoftwareVersion)
-- Import instance Safecopy HM.HashMap
import           Pos.Util                   (NamedMessagePart (..))

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

-- | ID of softwaree update proposal
type UpId = Hash UpdateProposal

-- | Proposal for software update
data UpdateProposal = UpdateProposal
    { upProtocolVersion :: !ProtocolVersion
    , upScriptVersion   :: !ScriptVersion
    , upSoftwareVersion :: !SoftwareVersion
    , upData            :: !(HM.HashMap SystemTag UpdateData)
    }
  deriving (Eq, Show, Generic, Typeable)

instance Buildable UpdateProposal where
    build UpdateProposal {..} =
      bprint (build%" { protocol v"%build%", scripts v"%build%", tags: "%listJson%" }")
        upSoftwareVersion upProtocolVersion upScriptVersion (HM.keys upData)

instance NamedMessagePart UpdateProposal where
    nMessageName _ = "Update proposal"

data UpdateData = UpdateData
    { udAppDiffHash :: !(Hash LByteString)
    , udPkgHash     :: !(Hash LByteString)
    , udUpdaterHash :: !(Hash LByteString)
    }
  deriving (Eq, Show, Generic, Typeable)

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
    }
  deriving (Eq, Show, Generic, Typeable)

-- | This type represents summary of votes issued by stakeholder.
data VoteState
    = PositiveVote    -- ^ Stakeholder voted once positively.
    | NegativeVote    -- ^ Stakeholder voted once positively.
    | PositiveRevote  -- ^ Stakeholder voted negatively, then positively.
    | NegativeRevote  -- ^ Stakeholder voted positively, then negatively.
    deriving (Show, Generic)

-- | Check whether given decision is a valid vote if applied to
-- existing vote (which may not exist).
canCombineVotes :: Bool -> Maybe VoteState -> Bool
canCombineVotes _ Nothing                 = True
canCombineVotes True (Just NegativeVote)  = True
canCombineVotes False (Just PositiveVote) = True
canCombineVotes _ _                       = False

-- | Apply decision to given vote (or Nothing). This function will
-- 'panic' if decision can't be applied. Use 'canCombineVotes' in
-- advance.
combineVotes :: Bool -> Maybe VoteState -> VoteState
combineVotes decision oldVote = assert (canCombineVotes decision oldVote) combineVotesDo
  where
    combineVotesDo =
        case (decision, oldVote) of
            (True, Nothing)            -> PositiveVote
            (False, Nothing)           -> NegativeVote
            (True, Just NegativeVote)  -> PositiveRevote
            (False, Just PositiveVote) -> NegativeRevote
            (_, Just vote)             -> onFailure vote
    onFailure =
        panic .
        sformat
        ("combineVotes: these votes can't be combined ("%shown%" and "%shown%")")
        decision

-- | Type alias for set of votes from stakeholders
type StakeholderVotes = HashMap PublicKey VoteState

-- | Tag for proposal messages
data ProposalMsgTag = ProposalMsgTag

instance NamedMessagePart ProposalMsgTag where
    nMessageName _ = "Update proposal tag"

instance Buildable ProposalMsgTag where
    build _ = "ProposalMsgTag"

-- | Tag for vote messages
data VoteMsgTag = VoteMsgTag

instance NamedMessagePart VoteMsgTag where
    nMessageName _ = "Update vote tag"

instance Buildable VoteMsgTag where
    build _ = "VoteMsgTag"

deriveSafeCopySimple 0 'base ''SystemTag
deriveSafeCopySimple 0 'base ''UpdateData
deriveSafeCopySimple 0 'base ''UpdateProposal
deriveSafeCopySimple 0 'base ''UpdateVote
