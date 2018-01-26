-- | Specification for submodules of Pos.Update.MemState

module Test.Pos.Update.MemStateSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Arbitrary.Update ()
import           Pos.Core (HasConfiguration)
import qualified Pos.Core.Update as Upd
import           Pos.Crypto (PublicKey, hash)
import qualified Pos.Update.MemState as Upd

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, (.&&.), (==>))

import           Test.Pos.Util (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "MemState" $ do
    describe "addToMemPool" $ do
        prop
            "applying an update payload to the mempool means all update votes are\
            \ properly added to it, and if it exists, the update proposal is added\
            \ correctly to the mempool as well."
            payloadIsAddedToMemPool
        prop
            "a vote for a proposal, by some public key, which was not included in the\
            \ payload's votes will not be accounted for in the mempool."
            badVoteIsNotAdded
        prop
            "if a key has no votes in neither the update payload's nor the payload's\
            \ proposal, then no votes by that key will be present in the mempool"
            keysWithoutVoteRemainSo

payloadIsAddedToMemPool :: HasConfiguration => Upd.UpdatePayload -> Upd.MemPool -> Property
payloadIsAddedToMemPool up@Upd.UpdatePayload {..} mp =
    proposalWasAdded .&&. votesWereAdded
  where
    proposalWasAdded = case upProposal of
        Nothing    -> True
        Just uProp -> HM.lookup (hash uProp) mpProposals == Just uProp
    votesWereAdded = all verifyVoteWasAdded upVotes
    verifyVoteWasAdded vote =
        (== Just vote) (HM.lookup (Upd.uvKey vote) =<<
                        HM.lookup (Upd.uvProposalId vote) mpLocalVotes)
    Upd.MemPool {..} = Upd.addToMemPool up mp

badVoteIsNotAdded :: HasConfiguration => Upd.UpdateVote -> Upd.UpdatePayload -> Upd.MemPool -> Property
badVoteIsNotAdded vote up@Upd.UpdatePayload {..} mp =
    (not $ elem vote upVotes) ==> voteIsNotPresent
  where
    Upd.MemPool {..} = Upd.addToMemPool up mp
    voteIsNotPresent =
        (/= Just vote) (HM.lookup (Upd.uvKey vote) =<<
                        HM.lookup (Upd.uvProposalId vote) mpLocalVotes)

keysWithoutVoteRemainSo :: HasConfiguration => PublicKey -> Upd.UpdatePayload -> Upd.MemPool -> Property
keysWithoutVoteRemainSo pk up@Upd.UpdatePayload {..} mp@(Upd.MemPool _ lv _) =
    keyHasNotVoted ==> keyHasNoNewVotes
  where
    Upd.MemPool {..} = Upd.addToMemPool up mp
    getThisKeysVotes k localVotes = HM.filter isJust . fmap (HM.lookup k) $ localVotes
    previousVotes = getThisKeysVotes pk lv
    currentVotes = getThisKeysVotes pk mpLocalVotes
    keyHasNotVoted = all ((/= pk) . Upd.uvKey) upVotes
    keyHasNoNewVotes = previousVotes == currentVotes
