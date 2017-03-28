-- | Functions which work with MemVar.

module Pos.Update.MemState.Functions
       ( withUSLock
       , addToMemPool
       ) where

import qualified Control.Concurrent.Lock   as Lock
import           Control.Monad.Catch       (MonadMask, bracket_)
import qualified Data.HashMap.Strict       as HM
import           Universum

import           Pos.Binary.Class          (Bi)
import           Pos.Crypto                (PublicKey, hash)
import           Pos.Update.Context        (UpdateContext (ucMemState))
import           Pos.Update.Core.Types     (LocalVotes, UpdatePayload (..),
                                            UpdateProposal, UpdateVote (..))
import           Pos.Update.MemState.Types (MemPool (..), MemVar (..))
import           Pos.Util.Context          (HasContext, askContext)

type UpdateVotes = HashMap PublicKey UpdateVote

withUSLock
    :: (HasContext UpdateContext m, MonadIO m, MonadMask m)
    => m a -> m a
withUSLock action = do
    lock <- mvLock <$> askContext ucMemState
    bracket_ (liftIO $ Lock.acquire lock) (liftIO $ Lock.release lock) action

-- | Add given payload to MemPool.
addToMemPool :: Bi UpdateProposal => UpdatePayload -> MemPool -> MemPool
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
