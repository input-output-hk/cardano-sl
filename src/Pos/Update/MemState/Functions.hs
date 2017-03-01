-- | Functions which work in MonadUSMem.

module Pos.Update.MemState.Functions
       ( withUSLock
       , addToMemPool
       ) where

import qualified Control.Concurrent.Lock   as Lock
import           Control.Monad.Catch       (MonadMask, bracket_)
import qualified Data.HashMap.Strict       as HM
import           Universum

import           Pos.Crypto                (PublicKey, hash)
import           Pos.Update.Core.Types     (LocalVotes, UpdatePayload (..),
                                            UpdateVote (..))
import           Pos.Update.MemState.Class (MonadUSMem (askUSMemVar))
import           Pos.Update.MemState.Types (MemPool (..), MemVar (..))

type UpdateVotes = HashMap PublicKey UpdateVote

withUSLock
    :: (MonadUSMem m, MonadIO m, MonadMask m)
    => m a -> m a
withUSLock action = do
    lock <- mvLock <$> askUSMemVar
    bracket_ (liftIO $ Lock.acquire lock) (liftIO $ Lock.release lock) action

-- | Add given payload to MemPool.
addToMemPool :: UpdatePayload -> MemPool -> MemPool
addToMemPool UpdatePayload {..} = addProposal . addVotes
  where
    addProposal mp =
        case upProposal of
            Nothing -> mp
            Just up -> mp {mpProposals = HM.insert (hash up) up (mpProposals mp)}
    addVotes mp = mp {mpLocalVotes = foldr' forceInsertVote (mpLocalVotes mp) upVotes}

    forceInsertVote :: UpdateVote -> LocalVotes -> LocalVotes
    forceInsertVote e@UpdateVote{..} = HM.alter (append e) uvProposalId

    append :: UpdateVote -> Maybe UpdateVotes -> Maybe UpdateVotes
    append e@UpdateVote{..} Nothing        = Just $ HM.singleton uvKey e
    append e@UpdateVote{..} (Just stVotes) = Just $ HM.insert uvKey e stVotes
