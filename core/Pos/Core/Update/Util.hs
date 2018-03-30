-- | Utility functions related to core update system types.

module Pos.Core.Update.Util
       (
         -- * Checkers/validators.
         checkUpdatePayload
       , checkUpdateProposal
       , checkUpdateVote
       , checkBlockVersionModifier
       , checkSoftforkRule

       , mkUpdateProposalWSign
       , mkVoteId
       , mkUpdateProof

       -- * Formatters
       , softforkRuleF

       -- * System tag helpers
       , archHelper
       , osHelper

       -- * Other utility functions
       , isBootstrapEraBVD
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Distribution.System (Arch (..), OS (..))
import           Distribution.Text (display)
import           Formatting (Format, build)
import           Instances.TH.Lift ()

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Crypto ()
import           Pos.Core.Common.Types (checkCoinPortion)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting (EpochIndex, isBootstrapEra)
import           Pos.Core.Update.Types (BlockVersion, BlockVersionData (..),
                                        BlockVersionModifier (..), SoftforkRule (..),
                                        SoftwareVersion, SystemTag, UpAttributes, UpdateData,
                                        UpdatePayload (..), UpdateProof, UpdateProposal (..),
                                        UpdateProposalToSign (..), UpdateVote (..), VoteId,
                                        checkSoftwareVersion, checkSystemTag)
import           Pos.Crypto (SafeSigner, SignTag (SignUSProposal, SignUSVote), checkSig, hash,
                             safeSign, safeToPublic)

checkUpdatePayload
    :: (HasConfiguration, MonadError Text m, Bi UpdateProposalToSign)
    => UpdatePayload
    -> m ()
checkUpdatePayload it = do
    -- Linter denies using foldables on Maybe.
    -- Suggests whenJust rather than forM_.
    --
    --   ¯\_(ツ)_/¯
    --
    whenJust (upProposal it) checkUpdateProposal
    forM_ (upVotes it) checkUpdateVote

checkBlockVersionModifier
    :: (MonadError Text m)
    => BlockVersionModifier
    -> m ()
checkBlockVersionModifier BlockVersionModifier {..} = do
    whenJust bvmMpcThd checkCoinPortion
    whenJust bvmHeavyDelThd checkCoinPortion
    whenJust bvmUpdateVoteThd checkCoinPortion
    whenJust bvmUpdateProposalThd checkCoinPortion
    whenJust bvmSoftforkRule checkSoftforkRule

checkSoftforkRule
    :: (MonadError Text m)
    => SoftforkRule
    -> m ()
checkSoftforkRule SoftforkRule {..} = do
    checkCoinPortion srInitThd
    checkCoinPortion srMinThd
    checkCoinPortion srThdDecrement

-- | 'SoftforkRule' formatter which restricts type.
softforkRuleF :: Format r (SoftforkRule -> r)
softforkRuleF = build

checkUpdateVote
    :: (HasConfiguration, MonadError Text m)
    => UpdateVote
    -> m ()
checkUpdateVote it =
    unless sigValid (throwError "UpdateVote: invalid signature")
  where
    sigValid = checkSig SignUSVote (uvKey it) (uvProposalId it, uvDecision it) (uvSignature it)

checkUpdateProposal
    :: (HasConfiguration, MonadError Text m, Bi UpdateProposalToSign)
    => UpdateProposal
    -> m ()
checkUpdateProposal it = do
    checkBlockVersionModifier (upBlockVersionMod it)
    checkSoftwareVersion (upSoftwareVersion it)
    forM_ (HM.keys (upData it)) checkSystemTag
    let toSign = UpdateProposalToSign
            (upBlockVersion it)
            (upBlockVersionMod it)
            (upSoftwareVersion it)
            (upData it)
            (upAttributes it)
    unless (checkSig SignUSProposal (upFrom it) toSign (upSignature it))
        (throwError "UpdateProposal: invalid signature")

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

-- | Version of 'isBootstrapEra' which takes 'BlockVersionData'
-- instead of unlock stake epoch.
isBootstrapEraBVD :: BlockVersionData -> EpochIndex -> Bool
isBootstrapEraBVD adoptedBVD = isBootstrapEra (bvdUnlockStakeEpoch adoptedBVD)
