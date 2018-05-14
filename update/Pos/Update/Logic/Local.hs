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
import           Control.Lens (views)
import           Control.Monad.Except (runExceptT, throwError)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Formatting (sformat, (%))
import           System.Wlog (WithLogger, logWarning)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Class (biSize)
import           Pos.Core (BlockVersionData (bvdMaxBlockSize), HeaderHash, HasGenesisBlockVersionData,
                           SlotId (..), slotIdF, HasProtocolConstants)
import           Pos.Core.Update (UpId, UpdatePayload (..), UpdateProposal, UpdateVote (..))
import           Pos.Crypto (PublicKey, shortHashF)
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.DB.GState.Common as DB
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Reporting (MonadReporting)
import           Pos.StateLock (StateLock)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Update.Context (UpdateContext (..))
import qualified Pos.Update.DB as DB
import           Pos.Update.MemState (LocalVotes, MemPool (..), MemState (..), MemVar (mvState),
                                      UpdateProposals, addToMemPool, withUSLock)
import           Pos.Update.Poll (MonadPoll (deactivateProposal), MonadPollRead (getProposal),
                                  PollModifier, PollVerFailure (..), evalPollT, execPollT,
                                  filterProposalsByThd, getAdoptedBV, modifyPollModifier,
                                  normalizePoll, refreshPoll, reportUnexpectedError, runDBPoll,
                                  runPollT, verifyAndApplyUSPayload)
import           Pos.Update.Poll.Types (canCombineVotes, psVotes)
import           Pos.Util.Util (HasLens (..), HasLens')

type USLocalLogicMode ctx m =
    ( MonadIO m
    , MonadDBRead m
    , MonadUnliftIO m
    , WithLogger m
    , MonadReader ctx m
    , HasLens UpdateContext ctx UpdateContext
    , HasLrcContext ctx
    , HasUpdateConfiguration
    )

type USLocalLogicModeWithLock ctx m =
    ( USLocalLogicMode ctx m
    , MonadMask m
    , HasLens' ctx StateLock
    )

getMemPool
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
    => m MemPool
getMemPool = msPool <$>
    (atomically . readTVar . mvState =<< views (lensOf @UpdateContext) ucMemState)

clearUSMemPool
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
    => m ()
clearUSMemPool =
    atomically . flip modifyTVar' resetData . mvState =<< views (lensOf @UpdateContext) ucMemState
  where
    resetData memState = memState {msPool = def, msModifier = def}

getPollModifier
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
    => m PollModifier
getPollModifier = msModifier <$>
    (atomically . readTVar . mvState =<< views (lensOf @UpdateContext) ucMemState)

getLocalProposals
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
    => m UpdateProposals
getLocalProposals = mpProposals <$> getMemPool

getLocalVotes
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
    => m LocalVotes
getLocalVotes = mpLocalVotes <$> getMemPool

-- Fetch memory state from 'TVar', modify it, write back. No
-- synchronization is done, it's caller's responsibility.
modifyMemState
    :: (MonadIO m, MonadReader ctx m, HasLens UpdateContext ctx UpdateContext)
    => (MemState -> m MemState) -> m ()
modifyMemState action = do
    stateVar <- mvState <$> views (lensOf @UpdateContext) ucMemState
    ms <- atomically $ readTVar stateVar
    newMS <- action ms
    atomically $ writeTVar stateVar newMS

----------------------------------------------------------------------------
-- Data exchange in general
----------------------------------------------------------------------------

processSkeleton ::
       ( USLocalLogicModeWithLock ctx m
       , MonadReporting ctx m
       , HasProtocolConstants
       , HasGenesisBlockVersionData
       )
    => UpdatePayload
    -> m (Either PollVerFailure ())
processSkeleton payload =
    reportUnexpectedError $
    withUSLock $
    runExceptT $
    modifyMemState $ \ms@MemState {..} -> do
        dbTip <- lift DB.getTip
        -- We must check tip here, because we can't be sure that tip
        -- in DB is the same as the tip in memory. Normally it will be
        -- the case, but if normalization fails, it won't be true.
        --
        -- If this equality holds, we can be sure that all further
        -- reads will be done for the same GState, because here we own
        -- global lock and nobody can modify GState.
        unless (dbTip == msTip) $ do
            let err = PollTipMismatch {ptmTipMemory = msTip, ptmTipDB = dbTip}
            throwError err
        maxBlockSize <- bvdMaxBlockSize <$> lift DB.getAdoptedBVData
        msIntermediate <-
            -- TODO: This is a rather arbitrary limit, we should revisit it (see CSL-1664)
            if | maxBlockSize * 2 <= mpSize msPool -> lift (refreshMemPool ms)
               | otherwise -> pure ms
        processSkeletonDo msIntermediate
  where
    processSkeletonDo ms@MemState {..} = do
        modifierOrFailure <-
            lift . runDBPoll . runExceptT . evalPollT msModifier . execPollT def $ do
                lastAdopted <- getAdoptedBV
                verifyAndApplyUSPayload lastAdopted True (Left msSlot) payload
        case modifierOrFailure of
            Left failure -> throwError failure
            Right modifier -> do
                let newModifier = modifyPollModifier msModifier modifier
                let newPool = addToMemPool payload msPool
                pure $ ms {msModifier = newModifier, msPool = newPool}

-- Remove most useless data from mem pool to make it smaller.
refreshMemPool
    :: ( MonadDBRead m
       , MonadUnliftIO m
       , MonadIO m
       , MonadReader ctx m
       , HasLens UpdateContext ctx UpdateContext
       , HasLrcContext ctx
       , WithLogger m
       , HasUpdateConfiguration
       , HasGenesisBlockVersionData
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
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
    => UpId -> m Bool
isProposalNeeded id = not . HM.member id <$> getLocalProposals

-- | Get update proposal with given id if it is known.
getLocalProposalNVotes
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
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
    :: ( USLocalLogicModeWithLock ctx m
       , MonadReporting ctx m
       , HasGenesisBlockVersionData
       , HasProtocolConstants
       )
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
    :: (USLocalLogicMode ctx m, HasGenesisBlockVersionData)
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
    :: (MonadReader ctx m, HasLens UpdateContext ctx UpdateContext, MonadIO m)
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
    :: ( USLocalLogicModeWithLock ctx m
       , MonadReporting ctx m
       , HasProtocolConstants
       , HasGenesisBlockVersionData
       )
    => UpdateVote -> m (Either PollVerFailure ())
processVote vote = processSkeleton $ UpdatePayload Nothing [vote]

----------------------------------------------------------------------------
-- Normalization and related
----------------------------------------------------------------------------

-- | Remove local data from memory state to make it consistent with
-- current GState.  This function assumes that GState is locked. It
-- tries to leave as much data as possible. It assumes that
-- 'stateLock' is taken.
usNormalize :: (USLocalLogicMode ctx m, HasGenesisBlockVersionData) => m ()
usNormalize = do
    tip <- DB.getTip
    stateVar <- mvState <$> views (lensOf @UpdateContext) ucMemState
    atomically . writeTVar stateVar =<< usNormalizeDo (Just tip) Nothing

-- Normalization under lock.  Note that here we don't care whether tip
-- in mempool is the same as the one is DB, because we take payload
-- from mempool and apply it to empty mempool, so it depends only on
-- GState.
usNormalizeDo
    :: (USLocalLogicMode ctx m, HasGenesisBlockVersionData)
    => Maybe HeaderHash -> Maybe SlotId -> m MemState
usNormalizeDo tip slot = do
    stateVar <- mvState <$> views (lensOf @UpdateContext) ucMemState
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
processNewSlot :: (USLocalLogicModeWithLock ctx m, HasGenesisBlockVersionData) => SlotId -> m ()
processNewSlot slotId = withUSLock $ processNewSlotNoLock slotId

processNewSlotNoLock :: (USLocalLogicMode ctx m, HasGenesisBlockVersionData) => SlotId -> m ()
processNewSlotNoLock slotId = modifyMemState $ \ms@MemState{..} -> do
    if | msSlot >= slotId -> pure ms
       -- Crucial changes happen only when epoch changes.
       | siEpoch msSlot == siEpoch slotId -> pure $ ms {msSlot = slotId}
       | otherwise -> usNormalizeDo Nothing (Just slotId)

-- | Prepare UpdatePayload for inclusion into new block with given
-- SlotId based on given tip.  This function assumes that
-- 'stateLock' is taken and nobody can apply/rollback blocks in
-- parallel or modify US mempool.  Sometimes payload can't be
-- created. It can happen if we are trying to create block for slot
-- which has already passed, for example. Or if we have different tip
-- in mempool because normalization failed earlier.
--
-- If we can't obtain payload for block creation, we use empty
-- payload, because it's important to create blocks for system
-- maintenance (empty blocks are better than no blocks).
usPreparePayload ::
       (USLocalLogicMode ctx m, HasGenesisBlockVersionData)
    => HeaderHash
    -> SlotId
    -> m UpdatePayload
usPreparePayload neededTip slotId@SlotId{..} = do
    -- First of all, we make sure that mem state corresponds to given
    -- slot.  If mem state corresponds to newer slot already, it won't
    -- be updated, but we don't want to create block in this case
    -- anyway.  In normal cases 'processNewSlot' can't fail here
    -- because of tip mismatch, because we are under 'stateLock'.
    processNewSlotNoLock slotId
    -- After that we normalize payload to be sure it's valid. We try
    -- to keep it valid anyway, but we decided to have an extra
    -- precaution. We also do it because here we need to eliminate all
    -- proposals which don't have enough positive stake for inclusion
    -- into block. We check that payload corresponds to requested slot
    -- and return it if it does.
    preparePayloadDo
  where
    preparePayloadDo = do
        -- Normalization is done just in case, as said before
        MemState {..} <- usNormalizeDo Nothing (Just slotId)
        -- If slot doesn't match, we can't provide payload for this slot.
        if | msSlot /= slotId -> def <$
               logWarning (sformat slotMismatchFmt msSlot slotId)
           | msTip /= neededTip -> def <$
               logWarning (sformat tipMismatchFmt msTip neededTip)
           | otherwise -> do
               -- Here we remove proposals which don't have enough
               -- positive stake for inclusion into payload.
               let MemPool {..} = msPool
               (filteredProposals, bad) <- runDBPoll . evalPollT msModifier $
                   filterProposalsByThd siEpoch mpProposals
               runDBPoll . evalPollT msModifier $
                   finishPrepare bad filteredProposals mpLocalVotes
    slotMismatchFmt = "US payload can't be created due to slot mismatch "%
                      "(our payload is for "%
                       slotIdF%", but requested one is "%slotIdF%")"
    tipMismatchFmt =  "US payload can't be created due to tip mismatch "
                     %"(our payload is for "
                     %shortHashF%", but we want to create payload based on tip "
                     %shortHashF%")"

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
    isVoteValid vote = do
        let id = uvProposalId vote
        proposalIsPresent <- isJust <$> getProposal id
        pure $ not (HS.member id badProposals) && proposalIsPresent
