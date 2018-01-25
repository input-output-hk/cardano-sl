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

       -- * System tag helpers
       , archHelper
       , osHelper
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Distribution.System (Arch (..), OS (..))
import           Distribution.Text (display)
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
    :: (HasConfiguration, Bi UpdateProposalToSign)
    => BlockVersion
    -> BlockVersionModifier
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> PublicKey
    -> Signature UpdateProposalToSign
    -> Either Text UpdateProposal
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
            Left "UpdateProposal: signature is invalid"
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
mkVoteId vote = (uvProposalId vote, uvKey vote, uvDecision vote)

mkUpdateProof
    :: Bi UpdatePayload
    => UpdatePayload -> UpdateProof
mkUpdateProof = hash

-- | Helper to turn an @OS@ into a @String@ compatible with the @systemTag@ previously
-- used in 'configuration.yaml'.
osHelper :: OS -> String
osHelper sys = case sys of
    Windows -> "win"
    OSX     -> "macos"
    Linux   -> "linux"
    _       -> display sys

-- | Helper to turn an @Arch@ into a @String@ compatible with the @systemTag@ previously
-- used in 'configuration.yaml'.
archHelper :: Arch -> String
archHelper archt = case archt of
    I386   -> "32"
    X86_64 -> "64"
    _      -> display archt
