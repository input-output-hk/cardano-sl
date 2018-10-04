module Pos.Core.Update.Vote
       ( UpdateVote (..)
       , mkUpdateVote
       , mkUpdateVoteSafe
       , formatVoteShort
       , shortVoteF
       , checkUpdateVote
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, bprint, build, builder, later, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Core.Common (addressHash)
import           Pos.Crypto (ProtocolMagic, PublicKey, SafeSigner, SecretKey,
                     SignTag (SignUSVote), Signature, checkSig, safeSign,
                     safeToPublic, shortHashF, sign, toPublic)

import           Pos.Core.Update.Proposal

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

deriveSafeCopySimple 0 'base ''UpdateVote
