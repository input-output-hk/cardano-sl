-- | Utility functions related to core update system types.

module Pos.Core.Update.Util
       (
         -- * Constructors
         mkUpdateProposal
       , mkUpdateProposalWSign
       , mkVoteId
       , mkUpdateProof

       -- * Formatters
       , softforkRuleF
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Formatting (Format, build)
import           Instances.TH.Lift ()

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Update.Types (BlockVersion, BlockVersionModifier (..), SoftforkRule,
                                        SoftwareVersion, SystemTag, UpAttributes, UpdateData,
                                        UpdatePayload, UpdateProof, UpdateProposal (..),
                                        UpdateProposalToSign (..), UpdateVote (..), VoteId)
import           Pos.Crypto (PublicKey, SafeSigner, SignTag (SignUSProposal), Signature, checkSig,
                             hash, safeSign, safeToPublic)

-- | 'SoftforkRule' formatter which restricts type.
softforkRuleF :: Format r (SoftforkRule -> r)
softforkRuleF = build

mkUpdateProposal
    :: (HasConfiguration, MonadFail m, Bi UpdateProposalToSign)
    => BlockVersion
    -> BlockVersionModifier
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> PublicKey
    -> Signature UpdateProposalToSign
    -> m UpdateProposal
mkUpdateProposal
    upBlockVersion
    upBlockVersionMod
    upSoftwareVersion
    upData
    upAttributes
    upFrom
    upSignature = do
        let toSign =
                UpdateProposalToSign
                    upBlockVersion
                    upBlockVersionMod
                    upSoftwareVersion
                    upData
                    upAttributes
        unless (checkSig SignUSProposal upFrom toSign upSignature) $
            fail $ "UpdateProposal: signature is invalid"
        pure UnsafeUpdateProposal{..}

mkUpdateProposalWSign
    :: (HasConfiguration, Bi UpdateProposalToSign)
    => BlockVersion
    -> BlockVersionModifier
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> SafeSigner
    -> UpdateProposal
mkUpdateProposalWSign upBlockVersion upBlockVersionMod upSoftwareVersion upData upAttributes ss =
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
    upSignature = safeSign SignUSProposal ss toSign

mkVoteId :: UpdateVote -> VoteId
mkVoteId UpdateVote{..} = (uvProposalId, uvKey, uvDecision)

mkUpdateProof
    :: Bi UpdatePayload
    => UpdatePayload -> UpdateProof
mkUpdateProof = hash
