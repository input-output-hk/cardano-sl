{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Logic of local data processing in Update System.

module Pos.Update.Logic.Local
       (
         -- * Proposals
         isProposalNeeded
       , getLocalProposal
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

import           Control.Concurrent.STM (readTVar, writeTVar)
import           Control.Lens           (at, (%=))
import           Data.Default           (Default (def))
import           Universum

import           Pos.Crypto             (PublicKey)
import           Pos.DB.Class           (MonadDB)
import           Pos.Types              (SlotId (siEpoch))
import           Pos.Update.Core        (UpId, UpdatePayload, UpdateProposal, UpdateVote)
import           Pos.Update.MemState    (MemPool (..), MemState (..), MemVar (..),
                                         MonadUSMem (askUSMemVar), modifyMemPool,
                                         modifyPollModifier, withUSLock)
import           Pos.Update.Poll        (PollModifier, PollVerFailure, ProposalState (..),
                                         UndecidedProposalState (..), execPollT,
                                         normalizePoll, pmNewActivePropsL, runDBPoll)
import           Pos.Util               (getKeys, zoom')

-- MonadMask is needed because are using Lock. It can be improved later.
type USLocalLogicMode σ m = (MonadDB σ m, MonadUSMem m, MonadMask m)

getMemPool :: (MonadUSMem m, MonadIO m) => m MemPool
getMemPool = msPool <$> (askUSMemState >>= atomically . readTVar)

getLocalProposals :: (MonadUSMem m, MonadIO m) => m UpdateProposals
getLocalProposals = mpProposals <$> getMemPool


getLocalVotes :: (MonadUSMem m, MonadIO m) => m GlobalVotes
getLocalVotes = mpGlobalVotes <$> getMemPool

----------------------------------------------------------------------------
-- Proposals
----------------------------------------------------------------------------

-- | This function returns true if update proposal with given
-- identifier should be requested.
isProposalNeeded :: (MonadIO m, MonadUSMem m) => UpId -> m Bool
isProposalNeeded id = not . HM.member id <$> getLocalProposals

-- | Get update proposal with given id if it is known.
getLocalProposal :: (MonadIO m, MonadUSMem m) => UpId -> m (Maybe UpdateProposal)
getLocalProposal id = HM.lookup id <$> getLocalProposals

-- | Process proposal received from network, checking it against
-- current state (global + local) and adding to local state if it's
-- valid with respect to it.
-- If proposal is added to store, 'Right ()' is returned.
-- Otherwise 'Left err' is returned and 'err' lets caller decide whether
-- sender could be sure that error would happen.
processProposal
    :: USLocalLogicMode θ m
    => UpdateProposal -> m (Either PollVerFailure ())
processProposal = notImplemented

----------------------------------------------------------------------------
-- Votes
----------------------------------------------------------------------------

-- | This function returns true if update vote proposal with given
-- identifier issued by stakeholder with given PublicKey and with
-- given decision should be requested.
isVoteNeeded
    :: (MonadIO m, MonadUSMem m)
    => UpId -> PublicKey -> Bool -> m Bool
isVoteNeeded propId pk decision =
    canCombineVotes decision . fmap snd . lookupVote <$> getLocalVotes
  where
    lookupVote locVotes = HM.lookup propId locVotes >>= HM.lookup pk

-- | Get update vote for proposal with given id from given issuer and
-- with given decision if it is known.
getLocalVote
    :: (MonadIO m, MonadUSMem m)
    => UpId -> PublicKey -> Bool -> m (Maybe UpdateVote)
getLocalVote propId pk decision = do
    vote <- lookupVote <$> getLocalVotes
    pure $
      if | canCombineVotes decision (snd <$> vote) -> Nothing
         | otherwise -> fst <$> vote
  where
    lookupVote locVotes = HM.lookup propId locVotes >>= HM.lookup pk

-- | Process vote received from network, checking it against
-- current state (global + local) and adding to local state if it's
-- valid with respect to it.
-- If vote is added to store, 'Right ()' is returned.
-- Otherwise 'Left err' is returned and 'err' lets caller decide whether
-- sender could be sure that error would happen.
processVote
    :: USLocalLogicMode ϟ m
    => UpdateVote -> m (Either PollVerFailure ())
processVote = notImplemented

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
processNewSlot slotId =
    withUSLock $ do
        stateVar <- mvState <$> askUSMemVar
        ms@MemState {..} <- atomically $ readTVar stateVar
        if | msSlot >= slotId -> pass
           | siEpoch msSlot == siEpoch slotId ->
               atomically $ writeTVar stateVar ms {msSlot = slotId}
           | otherwise -> processNewSlotDo stateVar ms
  where
    processNewSlotDo stateVar ms@MemState {..} = do
        let localUpIds = getKeys $ mpProposals msPool
        let invalidModifier = updateSlot slotId localUpIds msModifier
        normalizingModifier <-
            runDBPoll . execPollT msModifier . execPollT def $
            normalizePoll False
        let validModifier = modifyPollModifier normalizingModifier msModifier
        let validPool = modifyMemPool normalizingModifier msPool
        let newMS = ms {msModifier = validModifier, msPool = validPool}
        -- FIXME: check tip here.
        atomically $ writeTVar stateVar newMS

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
