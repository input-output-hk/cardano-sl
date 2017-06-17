{-# LANGUAGE ScopedTypeVariables #-}

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

       , clearUSMemPool
       ) where

import           Universum

import           Control.Concurrent.STM (modifyTVar', readTVar, writeTVar)
import           Control.Monad.Except   (runExceptT)
import           Data.Default           (Default (def))
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Ether
import           Formatting             (sformat, (%))
import           System.Wlog            (WithLogger, logWarning)

import           Pos.Binary.Class       (biSize)
import           Pos.Core               (BlockVersionData (bvdMaxBlockSize), HeaderHash,
                                         SlotId (..), slotIdF)
import           Pos.Core.Constants     (memPoolLimitRatio)
import           Pos.Crypto             (PublicKey)
import           Pos.DB.Class           (MonadDBRead, MonadRealDB)
import qualified Pos.DB.GState.Common   as DB
import           Pos.Lrc.Context        (LrcContext)
import           Pos.Update.Context     (UpdateContext (..))
import           Pos.Update.Core        (UpId, UpdatePayload (..), UpdateProposal,
                                         UpdateVote (..), canCombineVotes)
import qualified Pos.Update.DB          as DB
import           Pos.Update.MemState    (LocalVotes, MemPool (..), MemState (..),
                                         MemVar (mvState), UpdateProposals, addToMemPool,
                                         withUSLock)
import           Pos.Update.Poll        (MonadPoll (deactivateProposal),
                                         MonadPollRead (getProposal), PollModifier,
                                         PollVerFailure, evalPollT, execPollT,
                                         filterProposalsByThd, modifyPollModifier,
                                         normalizePoll, psVotes, refreshPoll, runDBPoll,
                                         runPollT, verifyAndApplyUSPayload)

-- MonadMask is needed because are using Lock. It can be improved later.
type USLocalLogicMode m =
    ( MonadRealDB m
    , MonadDBRead m
    , MonadMask m
    , WithLogger m
    , Ether.MonadReader' UpdateContext m
    , Ether.MonadReader' LrcContext m
    )

getMemPool
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => m MemPool
getMemPool = msPool <$>
    (atomically . readTVar . mvState =<< Ether.asks' ucMemState)

clearUSMemPool
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => m ()
clearUSMemPool =
    atomically . flip modifyTVar' resetData . mvState =<< Ether.asks' ucMemState
  where
    resetData memState = memState {msPool = def, msModifier = def}

getPollModifier
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => m PollModifier
getPollModifier = msModifier <$>
    (atomically . readTVar . mvState =<< Ether.asks' ucMemState)

getLocalProposals
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => m UpdateProposals
getLocalProposals = mpProposals <$> getMemPool

getLocalVotes
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => m LocalVotes
getLocalVotes = mpLocalVotes <$> getMemPool

withCurrentTip
    :: (Ether.MonadReader' UpdateContext m, MonadRealDB m, MonadDBRead m)
    => (MemState -> m MemState) -> m ()
withCurrentTip action = do
    tipBefore <- DB.getTip
    stateVar <- mvState <$> Ether.asks' ucMemState
    ms <- atomically $ readTVar stateVar
    newMS <- action ms
    atomically $ modifyTVar' stateVar $ \cur ->
      if | tipBefore == msTip cur -> newMS
         | otherwise -> cur

----------------------------------------------------------------------------
-- Data exchange in general
----------------------------------------------------------------------------

processSkeleton
    :: (USLocalLogicMode m)
    => UpdatePayload -> m (Either PollVerFailure ())
processSkeleton payload =
    withUSLock $
    runExceptT $
    withCurrentTip $ \ms@MemState {..} -> do
        maxBlockSize <- bvdMaxBlockSize <$> DB.getAdoptedBVData
        let maxMemPoolSize = maxBlockSize * memPoolLimitRatio
        msIntermediate <-
            if | maxMemPoolSize >= mpSize msPool -> refreshMemPool ms
               | otherwise -> pure ms
        processSkeletonDo msIntermediate
  where
    processSkeletonDo ms@MemState {..} = do
        modifier <-
            runDBPoll . evalPollT msModifier . execPollT def $
            verifyAndApplyUSPayload True (Left msSlot) payload
        let newModifier = modifyPollModifier msModifier modifier
        let newPool = addToMemPool payload msPool
        pure $ ms {msModifier = newModifier, msPool = newPool}

-- Remove most useless data from mem pool to make it smaller.
refreshMemPool
    :: ( MonadRealDB m
       , MonadDBRead m
       , Ether.MonadReader' UpdateContext m
       , Ether.MonadReader' LrcContext m
       , WithLogger m
       )
    => MemState -> m MemState
refreshMemPool ms@MemState {..} = do
    let MemPool {..} = msPool
    ((newProposals, newVotes), newModifier) <-
        runDBPoll . runPollT def $ refreshPoll msSlot mpProposals mpLocalVotes
    let newPool =
            MemPool
            { mpProposals = newProposals
            , mpLocalVotes = newVotes
            , mpSize = biSize newProposals + biSize newVotes
            }
    return ms {msModifier = newModifier, msPool = newPool}

----------------------------------------------------------------------------
-- Proposals
----------------------------------------------------------------------------

-- | This function returns true if update proposal with given
-- identifier should be requested.
isProposalNeeded
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => UpId -> m Bool
isProposalNeeded id = not . HM.member id <$> getLocalProposals

-- | Get update proposal with given id if it is known.
getLocalProposalNVotes
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => UpId -> m (Maybe (UpdateProposal, [UpdateVote]))
getLocalProposalNVotes id = do
    prop <- HM.lookup id <$> getLocalProposals
    votes <- getLocalVotes
    pure $
        case prop of
            Nothing -> Nothing
            Just p  -> Just (p, toList $ HM.lookupDefault mempty id votes)

-- | Process proposal received from network, checking it against
-- current state (global + local) and adding to local state if it's
-- valid with respect to it.
-- If proposal is added to store, 'Right ()' is returned.
-- Otherwise 'Left err' is returned and 'err' lets caller decide whether
-- sender could be sure that error would happen.
processProposal
    :: (USLocalLogicMode m)
    => UpdateProposal -> m (Either PollVerFailure ())
processProposal proposal = processSkeleton $ UpdatePayload (Just proposal) []

----------------------------------------------------------------------------
-- Votes
----------------------------------------------------------------------------

lookupVote :: UpId -> PublicKey -> LocalVotes -> Maybe UpdateVote
lookupVote propId pk locVotes = HM.lookup propId locVotes >>= HM.lookup pk

-- | This function returns true if update vote proposal with given
-- identifier issued by stakeholder with given PublicKey and with
-- given decision should be requested.
isVoteNeeded
    :: USLocalLogicMode m
    => UpId -> PublicKey -> Bool -> m Bool
isVoteNeeded propId pk decision = do
    modifier <- getPollModifier
    runDBPoll . evalPollT modifier $ do
        proposal <- getProposal propId
        case proposal of
            Nothing -> pure False
            Just ps  -> pure .
                canCombineVotes decision .
                HM.lookup pk .
                psVotes $ ps

-- | Get update vote for proposal with given id from given issuer and
-- with given decision if it is known.
getLocalVote
    :: (Ether.MonadReader' UpdateContext m, MonadIO m)
    => UpId -> PublicKey -> Bool -> m (Maybe UpdateVote)
getLocalVote propId pk decision = do
    voteMaybe <- lookupVote propId pk <$> getLocalVotes
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
    :: (USLocalLogicMode m)
    => UpdateVote -> m (Either PollVerFailure ())
processVote vote = processSkeleton $ UpdatePayload Nothing [vote]

----------------------------------------------------------------------------
-- Normalization and related
----------------------------------------------------------------------------

-- | Remove local data from memory state to make it consistent with
-- current GState.  This function assumes that GState is locked. It
-- tries to leave as much data as possible. It assumes that
-- 'blkSemaphore' is taken.
usNormalize :: (USLocalLogicMode m) => m ()
usNormalize =
    withUSLock $ do
        tip <- DB.getTip
        stateVar <- mvState <$> Ether.asks' ucMemState
        atomically . writeTVar stateVar =<< usNormalizeDo (Just tip) Nothing

-- Normalization under lock.
usNormalizeDo
    :: (USLocalLogicMode m)
    => Maybe HeaderHash -> Maybe SlotId -> m MemState
usNormalizeDo tip slot = do
    stateVar <- mvState <$> Ether.asks' ucMemState
    ms@MemState {..} <- atomically $ readTVar stateVar
    let MemPool {..} = msPool
    ((newProposals, newVotes), newModifier) <-
        runDBPoll . runPollT def $ normalizePoll msSlot mpProposals mpLocalVotes
    let newTip = fromMaybe msTip tip
    let newSlot = fromMaybe msSlot slot
    let newPool =
            MemPool
            { mpProposals = newProposals
            , mpLocalVotes = newVotes
            , mpSize = biSize newProposals + biSize newVotes
            }
    let newMS =
            ms
            { msModifier = newModifier
            , msPool = newPool
            , msTip = newTip
            , msSlot = newSlot
            }
    return newMS

-- | Update memory state to make it correct for given slot.
processNewSlot :: (USLocalLogicMode m) => SlotId -> m ()
processNewSlot slotId = withUSLock $ withCurrentTip $ \ms@MemState{..} -> do
    if | msSlot >= slotId -> pure ms
       -- Crucial changes happen only when epoch changes.
       | siEpoch msSlot == siEpoch slotId -> pure $ ms {msSlot = slotId}
       | otherwise -> usNormalizeDo Nothing (Just slotId)

-- | Prepare UpdatePayload for inclusion into new block with given
-- SlotId.  This function assumes that 'blkSemaphore' is taken and
-- nobody can apply/rollback blocks in parallel.
-- Sometimes payload can't be created. It can happen if we are trying to
-- create block for slot which has already passed, for example.
usPreparePayload :: (USLocalLogicMode m) => SlotId -> m (Maybe UpdatePayload)
usPreparePayload slotId@SlotId{..} = do
    -- First of all, we make sure that mem state corresponds to given
    -- slot.  If mem state corresponds to newer slot already, it won't
    -- be updated, but we don't want to create block in this case
    -- anyway.  Here 'processNewSlot' can't fail because of tip
    -- mismatch, because we are under 'blkSemaphore'.
    processNewSlot slotId
    -- After that we normalize payload to be sure it's valid. We try
    -- to keep it valid anyway, but we decided to have an extra
    -- precaution. We also do it because here we need to eliminate all
    -- proposals which don't have enough positive stake for inclusion
    -- into block. We check that payload corresponds to requested slot
    -- and return it if it does. We take lock to be sure that noone
    -- can put anything during normalization.
    withUSLock preparePayloadDo
  where
    preparePayloadDo = do
        -- Normalization is done just in case, as said before
        MemState {..} <- usNormalizeDo Nothing (Just slotId)
        -- If slot doesn't match, we can't provide payload for this slot.
        if | msSlot /= slotId -> Nothing <$
               logWarning (sformat slotMismatchFmt msSlot slotId)
           | otherwise -> do
               -- Here we remove proposals which don't have enough
               -- positive stake for inclusion into payload.
               let MemPool {..} = msPool
               (filteredProposals, bad) <- runDBPoll . evalPollT msModifier $
                   filterProposalsByThd siEpoch mpProposals
               fmap Just . runDBPoll . evalPollT msModifier $
                   finishPrepare bad filteredProposals mpLocalVotes
    slotMismatchFmt = "US payload can't be created due to slot mismatch "%
                      "(our payload is for "%
                       slotIdF%", but requested one is "%slotIdF%")"

-- Here we basically choose only one proposal for inclusion and remove
-- all votes for other proposals.
finishPrepare
    :: MonadPoll m
    => HashSet UpId -> UpdateProposals -> LocalVotes -> m UpdatePayload
finishPrepare badProposals proposals votes = do
    proposalPair <- foldM findProposal Nothing $ HM.toList proposals
    mapM_ (deactivate (fst <$> proposalPair)) $ HM.toList proposals
    let allVotes :: [UpdateVote]
        allVotes = concatMap toList $ toList votes
    goodVotes <- filterM isVoteValid allVotes
    return $
        UpdatePayload {upProposal = snd <$> proposalPair, upVotes = goodVotes}
  where
    findProposal (Just x) _ = pure (Just x)
    findProposal Nothing x@(upId, _) =
        bool Nothing (Just x) <$> (isJust <$> getProposal upId)
    deactivate chosenUpId (upId, _)
        | chosenUpId == Just upId = pass
        | otherwise =
            deactivateProposal upId
    isVoteValid UpdateVote {..} =
        (not (HS.member uvProposalId badProposals) &&) . isJust <$>
        getProposal uvProposalId
