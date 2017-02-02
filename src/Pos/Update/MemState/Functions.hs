-- | Functions which work in MonadUSMem.

module Pos.Update.MemState.Functions
       ( withUSLock
       , modifyMemPool
       ) where

import qualified Control.Concurrent.Lock   as Lock
import           Control.Monad.Catch       (MonadMask, bracket_)
import qualified Data.HashMap.Strict       as HM
import           Universum

import           Pos.Crypto                (PublicKey, hash)
import           Pos.Update.Core.Types     (LocalVotes, UpId, UpdatePayload (..),
                                            UpdateProposal, UpdateVote (..))
import           Pos.Update.MemState.Class (MonadUSMem (askUSMemVar))
import           Pos.Update.MemState.Types (MemPool (..), MemVar (..))
import           Pos.Update.Poll.Types     (PollModifier (..), ProposalState, psProposal,
                                            psVotes)

type UpdateVotes = HashMap PublicKey UpdateVote

withUSLock
    :: (MonadUSMem m, MonadIO m, MonadMask m)
    => m a -> m a
withUSLock action = do
    lock <- mvLock <$> askUSMemVar
    bracket_ (liftIO $ Lock.acquire lock) (liftIO $ Lock.release lock) action

-- | Modify MemPool using UpdatePayload and PollModifier.
--
-- UpdatePayload is used to add new data to MemPool. Data must be
-- verified by caller. It's added directly to MemPool.
--
-- PollModifier is used to remove or modify some data in MemPool.
-- All deleted proposals and votes for them are removed.
-- TODO [CSL-625] Deleted and modified votes from non-deleted proposals
-- are not handled properly.
modifyMemPool :: UpdatePayload -> PollModifier -> MemPool -> MemPool
modifyMemPool UpdatePayload {..} PollModifier{..} =
     addModifiers . delModifiers . addProposal upProposal
  where
    delModifiers :: MemPool -> MemPool
    delModifiers MemPool{..} = MemPool
        (foldr' HM.delete mpProposals pmDelActiveProps)
        (foldr' HM.delete mpLocalVotes pmDelActiveProps)

    addModifiers :: MemPool -> MemPool
    addModifiers MemPool{..} =
        let removeNotPresentedInActiveProps :: UpId -> UpdateVotes -> UpdateVotes
            removeNotPresentedInActiveProps id stVotes
                | Just activeProposal <- HM.lookup id pmNewActiveProps =
                    stVotes `HM.intersection` (psVotes activeProposal)
                | otherwise = stVotes

            filteredLocalVotes :: LocalVotes
            filteredLocalVotes = HM.mapWithKey removeNotPresentedInActiveProps mpLocalVotes
        in MemPool
              (foldr' (uncurry HM.insert) mpProposals
                  (HM.toList $ HM.map psProposal pmNewActiveProps))
        (foldr' forceInsertVote filteredLocalVotes .
             filter (memberVS pmNewActiveProps) $ upVotes)

    addProposal :: Maybe UpdateProposal -> MemPool -> MemPool
    addProposal Nothing  mp = mp
    addProposal (Just p) MemPool {..} = MemPool
        (HM.insert (hash p) p mpProposals)
        mpLocalVotes

    forceInsertVote :: UpdateVote -> LocalVotes -> LocalVotes
    forceInsertVote e@UpdateVote{..} = HM.alter (append e) uvProposalId

    append :: UpdateVote -> Maybe UpdateVotes -> Maybe UpdateVotes
    append e@UpdateVote{..} Nothing        = Just $ HM.singleton uvKey e
    append e@UpdateVote{..} (Just stVotes) = Just $ HM.insert uvKey e stVotes

    memberVS :: HashMap UpId ProposalState -> UpdateVote -> Bool
    memberVS activeProps UpdateVote{..} =
        maybe False (HM.member uvKey . psVotes) $ HM.lookup uvProposalId activeProps

    -- I'll remove it, if it won't be needed.
    -- safeInsertVote :: ExtendedUpdateVote -> LocalVotes -> LocalVotes
    -- safeInsertVote e@(UpdateVote{..}, _) = HM.alter (insertOrRevote e) uvProposalId

    -- insertOrRevote :: ExtendedUpdateVote -> Maybe ExtStakeholderVotes -> Maybe ExtStakeholderVotes
    -- insertOrRevote e@(UpdateVote{..}, _) Nothing        = Just $ HM.singleton uvKey e
    -- insertOrRevote e@(UpdateVote{..}, _) (Just stVotes) = Just $ HM.alter (voteOrRevote e) uvKey stVotes

    -- voteOrRevote :: ExtendedUpdateVote -> Maybe ExtendedUpdateVote -> Maybe ExtendedUpdateVote
    -- voteOrRevote (uv, vs) (fmap snd -> mb) = Just (uv, ) <*> -- we remove vote if it can't be combined
    --     (combineVotes (isPositiveVote vs) mb)
