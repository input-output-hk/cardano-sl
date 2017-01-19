-- | Functions which work in MonadUSMem.

module Pos.Update.MemState.Functions
       ( withUSLock
       , modifyMemPool
       ) where

import qualified Control.Concurrent.Lock      as Lock
import           Control.Monad.Catch          (MonadMask, bracket_)
import qualified Data.HashMap.Strict          as HM
import           Universum

import           Pos.Crypto                   (hash)
import           Pos.Update.Core.Types        (ExtStakeholderVotes, ExtendedUpdateVote,
                                               LocalVotes, UpId, UpdatePayload (..),
                                               UpdateProposal, UpdateVote (..), VoteState)
import           Pos.Update.MemState.Class    (MonadUSMem (askUSMemVar))
import           Pos.Update.MemState.MemState (MemVar (..))
import           Pos.Update.MemState.Types    (MemPool (..))
import           Pos.Update.Poll.Types        (PollModifier (..), ProposalState,
                                               psProposal, psVotes)

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
    addModifiers MemPool{..} = MemPool
        (foldr' (uncurry HM.insert) mpProposals
             (HM.toList $ HM.map psProposal pmNewActiveProps))
        (foldr' insertVote mpLocalVotes .
             mapMaybe (\x -> (x,) <$> lookupVS pmNewActiveProps x) $ upVotes)

    addProposal :: Maybe UpdateProposal -> MemPool -> MemPool
    addProposal Nothing  mp = mp
    addProposal (Just p) MemPool {..} = MemPool
        (HM.insert (hash p) p mpProposals)
        mpLocalVotes

    lookupVS :: HashMap UpId ProposalState -> UpdateVote -> Maybe VoteState
    lookupVS activeProps UpdateVote{..} =
        HM.lookup uvProposalId activeProps >>= HM.lookup uvKey . psVotes

    insertVote :: ExtendedUpdateVote -> LocalVotes -> LocalVotes
    insertVote e@(UpdateVote{..}, _) = HM.alter (append e) uvProposalId

    append :: ExtendedUpdateVote -> Maybe ExtStakeholderVotes -> Maybe ExtStakeholderVotes
    append e@(UpdateVote{..}, _) Nothing        = Just $ HM.singleton uvKey e
    append e@(UpdateVote{..}, _) (Just stVotes) = Just $ HM.insert uvKey e stVotes
