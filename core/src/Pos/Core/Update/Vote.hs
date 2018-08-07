module Pos.Core.Update.Vote
       (
       -- Software update proposal
         UpdateProposal (..)
       , UpdateProposals
       , UpId
       , UpAttributes
       , UpdateProposalToSign (..)
       , formatMaybeProposal
       , mkUpdateProposalWSign
       , checkUpdateProposal

       -- Software update vote
       , VoteId
       , UpdateVote (..)
       , mkUpdateVote
       , mkUpdateVoteSafe
       , formatVoteShort
       , shortVoteF
       , checkUpdateVote
       , mkVoteId
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, bprint, build, builder, later, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                     deriveSimpleBi, encodeListLen, enforceSize)
import           Pos.Core.Attributes (Attributes, areAttributesKnown)
import           Pos.Core.Common (addressHash)
import           Pos.Crypto (Hash, ProtocolMagic, PublicKey, SafeSigner,
                     SecretKey, SignTag (SignUSProposal, SignUSVote),
                     Signature, checkSig, hash, safeSign, safeToPublic,
                     shortHashF, sign, toPublic)

import           Pos.Core.Update.BlockVersion
import           Pos.Core.Update.BlockVersionModifier
import           Pos.Core.Update.Data
import           Pos.Core.Update.SoftwareVersion
import           Pos.Core.Update.SystemTag

----------------------------------------------------------------------------
-- Software Update Proposal
----------------------------------------------------------------------------

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

instance Buildable UpdateProposal where
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

instance NFData UpdateProposal

instance Bi UpdateProposal where
    encode up = encodeListLen 7
            <> encode (upBlockVersion up)
            <> encode (upBlockVersionMod up)
            <> encode (upSoftwareVersion up)
            <> encode (upData up)
            <> encode (upAttributes up)
            <> encode (upFrom up)
            <> encode (upSignature up)
    decode = do
        enforceSize "UpdateProposal" 7
        UnsafeUpdateProposal <$> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode

formatMaybeProposal :: Maybe UpdateProposal -> Builder
formatMaybeProposal = maybe "no proposal" Buildable.build

mkUpdateProposalWSign
    :: ProtocolMagic
    -> BlockVersion
    -> BlockVersionModifier
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> SafeSigner
    -> UpdateProposal
mkUpdateProposalWSign pm upBlockVersion upBlockVersionMod upSoftwareVersion upData upAttributes ss =
    UnsafeUpdateProposal {..}
  where
    toSign =
        UpdateProposalToSign
            upBlockVersion
            upBlockVersionMod
            upSoftwareVersion
            upData
            upAttributes
    upFrom = safeToPublic ss
    upSignature = safeSign pm SignUSProposal ss toSign

checkUpdateProposal
    :: MonadError Text m
    => ProtocolMagic
    -> UpdateProposal
    -> m ()
checkUpdateProposal pm it = do
    checkBlockVersionModifier (upBlockVersionMod it)
    checkSoftwareVersion (upSoftwareVersion it)
    forM_ (HM.keys (upData it)) checkSystemTag
    let toSign = UpdateProposalToSign
            (upBlockVersion it)
            (upBlockVersionMod it)
            (upSoftwareVersion it)
            (upData it)
            (upAttributes it)
    unless (checkSig pm SignUSProposal (upFrom it) toSign (upSignature it))
        (throwError "UpdateProposal: invalid signature")

----------------------------------------------------------------------------
-- Software Update Vote
----------------------------------------------------------------------------

-- | ID of a voter and its decision regarding a specific software update
-- proposal
type VoteId = (UpId, PublicKey, Bool)

instance Buildable VoteId where
    build (upId, pk, dec) =
      bprint ("Vote Id { voter: "%build%", proposal id: "%build%", voter's decision: "%build%" }")
             pk upId dec

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

instance Buildable (UpdateProposal, [UpdateVote]) where
    build (up, votes) =
        bprint
            (build % " with votes: " %listJson)
            up
            (map formatVoteShort votes)

instance Bi UpdateVote where
    encode uv =  encodeListLen 4
            <> encode (uvKey uv)
            <> encode (uvProposalId uv)
            <> encode (uvDecision uv)
            <> encode (uvSignature uv)
    decode = do
        enforceSize "UpdateVote" 4
        uvKey        <- decode
        uvProposalId <- decode
        uvDecision   <- decode
        uvSignature  <- decode
        pure UnsafeUpdateVote{..}

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

checkUpdateVote
    :: (MonadError Text m)
    => ProtocolMagic
    -> UpdateVote
    -> m ()
checkUpdateVote pm it =
    unless sigValid (throwError "UpdateVote: invalid signature")
  where
    sigValid = checkSig pm SignUSVote (uvKey it) (uvProposalId it, uvDecision it) (uvSignature it)

mkVoteId :: UpdateVote -> VoteId
mkVoteId vote = (uvProposalId vote, uvKey vote, uvDecision vote)

----------------------------------------------------------------------------
-- TH generated instances at the end of the file.
----------------------------------------------------------------------------

deriveSimpleBi ''UpdateProposalToSign [
    Cons 'UpdateProposalToSign [
        Field [| upsBV   :: BlockVersion                     |],
        Field [| upsBVM  :: BlockVersionModifier           |],
        Field [| upsSV   :: SoftwareVersion                  |],
        Field [| upsData :: HashMap SystemTag UpdateData |],
        Field [| upsAttr :: UpAttributes                   |]
    ]]

deriveSafeCopySimple 0 'base ''UpdateProposal

deriveSafeCopySimple 0 'base ''UpdateVote
