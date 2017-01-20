{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Logic of local data processing in Update System.

module Pos.Update.Logic.Local
       (
         -- * Proposals
         isProposalNeeded
       , getLocalProposalNVotes
       , processProposal

         -- * Votes
       , isVoteNeeded
       , getLocalVote
       , processVote

         -- * Normalization
       , usNormalize
       , processNewSlot
       , usPreparePayload
       ) where

import           Control.Concurrent.STM (modifyTVar', readTVar)
import           Control.Lens           (at, (%=))
import           Control.Monad.Except   (runExceptT)
import           Data.Default           (Default (def))
import qualified Data.HashMap.Strict    as HM
import           Universum

import           Pos.Crypto             (PublicKey)
import           Pos.DB.Class           (MonadDB)
import qualified Pos.DB.GState          as DB
import           Pos.Types              (SlotId (siEpoch))
import           Pos.Update.Core        (UpId, UpdatePayload (..), UpdateProposal,
                                         UpdateVote (..), VoteState, canCombineVotes)
import           Pos.Update.MemState    (LocalVotes, MemPool (..), MemState (..),
                                         MonadUSMem, UpdateProposals, askUSMemState,
                                         modifyMemPool, withUSLock)
import           Pos.Update.Poll        (PollModifier, PollVerFailure, ProposalState (..),
                                         UndecidedProposalState (..), evalPollT,
                                         execPollT, modifyPollModifier, normalizePoll,
                                         pmNewActivePropsL, runDBPoll,
                                         verifyAndApplyUSPayload)
import           Pos.Util               (getKeys, zoom')

-- MonadMask is needed because are using Lock. It can be improved later.
type USLocalLogicMode σ m = (MonadDB σ m, MonadUSMem m, MonadMask m)

getMemPool :: (MonadUSMem m, MonadIO m) => m MemPool
getMemPool = msPool <$> (askUSMemState >>= atomically . readTVar)

getLocalProposals :: (MonadUSMem m, MonadIO m) => m UpdateProposals
getLocalProposals = mpProposals <$> getMemPool

getLocalVotes :: (MonadUSMem m, MonadIO m) => m LocalVotes
getLocalVotes = mpLocalVotes <$> getMemPool

----------------------------------------------------------------------------
-- Proposals
----------------------------------------------------------------------------

-- | This function returns true if update proposal with given
-- identifier should be requested.
isProposalNeeded :: (MonadIO m, MonadUSMem m) => UpId -> m Bool
isProposalNeeded id = not . HM.member id <$> getLocalProposals

-- | Get update proposal with given id if it is known.
getLocalProposalNVotes :: (MonadIO m, MonadUSMem m) => UpId -> m (Maybe (UpdateProposal, [UpdateVote]))
getLocalProposalNVotes id = do
    prop <- HM.lookup id <$> getLocalProposals
    votes <- getLocalVotes
    pure $
        case prop of
            Nothing -> Nothing
            Just p  -> Just (p, map fst . toList $ HM.lookupDefault mempty id votes)

-- | Process proposal received from network, checking it against
-- current state (global + local) and adding to local state if it's
-- valid with respect to it.
-- If proposal is added to store, 'Right ()' is returned.
-- Otherwise 'Left err' is returned and 'err' lets caller decide whether
-- sender could be sure that error would happen.
processProposal
    :: USLocalLogicMode θ m
    => UpdateProposal -> m (Either PollVerFailure ())
processProposal proposal = processSkeleton $ UpdatePayload (Just proposal) []

----------------------------------------------------------------------------
-- Votes
----------------------------------------------------------------------------

lookupVote :: UpId -> PublicKey -> LocalVotes -> Maybe (UpdateVote, VoteState)
lookupVote propId pk locVotes = HM.lookup propId locVotes >>= HM.lookup pk

-- | This function returns true if update vote proposal with given
-- identifier issued by stakeholder with given PublicKey and with
-- given decision should be requested.
isVoteNeeded
    :: (MonadIO m, MonadUSMem m)
    => UpId -> PublicKey -> Bool -> m Bool
isVoteNeeded propId pk decision =
    canCombineVotes decision . fmap snd . lookupVote propId pk <$> getLocalVotes

-- | Get update vote for proposal with given id from given issuer and
-- with given decision if it is known.
getLocalVote
    :: (MonadIO m, MonadUSMem m)
    => UpId -> PublicKey -> Bool -> m (Maybe UpdateVote)
getLocalVote propId pk decision = do
    voteMaybe <- fmap fst . lookupVote propId pk <$> getLocalVotes
    pure $
        case voteMaybe of
            Nothing -> Nothing
            Just vote
                | uvDecision vote == decision -> Just vote
                | otherwise -> Nothing

-- | Process vote received from network, checking it against
-- current state (global + local) and adding to local state if it's
-- valid with respect to it.
-- If vote is added to store, 'Right ()' is returned.
-- Otherwise 'Left err' is returned and 'err' lets caller decide whether
-- sender could be sure that error would happen.
processVote
    :: USLocalLogicMode ϟ m
    => UpdateVote -> m (Either PollVerFailure ())
processVote vote = processSkeleton $ UpdatePayload Nothing [vote]

withCurrentTip :: (MonadDB ssc m, MonadUSMem m) => (MemState -> m MemState) -> m ()
withCurrentTip action = do
    tipBefore <- DB.getTip
    stateVar <- askUSMemState
    ms <- atomically $ readTVar stateVar
    newMS <- action ms
    atomically $ modifyTVar' stateVar $ \cur ->
      if | tipBefore == msTip cur -> newMS
         | otherwise -> cur

processSkeleton
    :: USLocalLogicMode ϟ m
    => UpdatePayload -> m (Either PollVerFailure ())
processSkeleton payload = withUSLock $ runExceptT $ withCurrentTip $ \ms@MemState{..} -> do
    modifier <-
        runDBPoll . evalPollT msModifier . execPollT def $
        verifyAndApplyUSPayload False (Left msSlot) payload
    let newModifier = modifyPollModifier msModifier modifier
    let newPool = modifyMemPool payload modifier msPool
    pure $ ms {msModifier = newModifier, msPool = newPool}

----------------------------------------------------------------------------
-- Normalization
----------------------------------------------------------------------------

-- | Remove local data from memory state to make it consistent with
-- current GState.  This function assumes that GState is locked. It
-- tries to leave as much data as possible.
usNormalize :: USLocalLogicMode ς m => m ()
usNormalize = const pass notImplemented

-- | Update memory state to make it correct for given slot.
processNewSlot :: USLocalLogicMode μ m => SlotId -> m ()
processNewSlot slotId = withUSLock $ withCurrentTip $ \ms@MemState{..} -> do
    if | msSlot >= slotId -> pure ms
       | siEpoch msSlot == siEpoch slotId -> pure $ ms {msSlot = slotId}
       | otherwise -> processNewSlotDo ms
  where
    processNewSlotDo ms@MemState{..} = do
        let localUpIds = getKeys $ mpProposals msPool
        let slotModifier = updateSlot slotId localUpIds msModifier
        normalizingModifier <-
            runDBPoll . evalPollT slotModifier . execPollT def $
            normalizePoll False
        let validModifier = modifyPollModifier msModifier normalizingModifier
        let validPool = modifyMemPool def normalizingModifier msPool
        pure $ ms { msModifier = validModifier
                  , msPool = validPool
                  , msSlot = slotId}

updateSlot :: SlotId -> HashSet UpId -> PollModifier -> PollModifier
updateSlot newSlot localIds =
    execState $ zoom' pmNewActivePropsL $ mapM_ updateSlotSingle localIds
  where
    updateSlotSingle upId = at upId %= updateSlotSingleF
    updateSlotSingleF Nothing                  = Nothing
    updateSlotSingleF (Just (PSUndecided ups)) = Just $ PSUndecided ups {upsSlot = newSlot}
    updateSlotSingleF (Just decided)           = Just decided

-- | Prepare UpdatePayload for inclusion into new block with given SlotId.
usPreparePayload :: USLocalLogicMode μ m => SlotId -> m UpdatePayload
usPreparePayload _ = const (pure def) notImplemented
