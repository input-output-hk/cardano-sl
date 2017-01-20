{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions which operate on MonadPoll[Read].

module Pos.Update.Poll.Functions
       ( verifyAndApplyUSPayload
       , rollbackUSPayload
       , normalizePoll
       ) where

import           Control.Lens          (at)
import           Control.Monad.Except  (MonadError, throwError)
import qualified Data.HashMap.Strict   as HM
import           Data.List             (partition)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NE
import           Exceptions            (note)
import           Universum

import           Pos.Constants         (blkSecurityParam, updateImplicitApproval,
                                        updateProposalThreshold, updateVoteThreshold)
import           Pos.Crypto            (PublicKey, hash)
import           Pos.Types             (ChainDifficulty, Coin, EpochIndex,
                                        MainBlockHeader, SlotId (siEpoch),
                                        SoftwareVersion (..), addressHash,
                                        applyCoinPortion, coinToInteger, difficultyL,
                                        epochIndexL, flattenSlotId, headerSlot, sumCoins,
                                        unflattenSlotId, unsafeAddCoin,
                                        unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Types.Version     (ApplicationName, NumSoftwareVersion)
import           Pos.Update.Core       (UpId, UpdatePayload (..), UpdateProposal (..),
                                        UpdateVote (..), combineVotes, isPositiveVote,
                                        newVoteState)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types (DecidedProposalState (..), PollVerFailure (..),
                                        PrevValue (..), ProposalState (..), USUndo (..),
                                        UndecidedProposalState (..))
----------------------------------------------------------------------------
-- Primitive operations, helpers
----------------------------------------------------------------------------

newtype TotalPositive = TotalPositive Integer
newtype TotalNegative = TotalNegative Integer
newtype TotalSum = TotalSum Integer

mkTotPositive :: Coin -> TotalPositive
mkTotPositive = TotalPositive . coinToInteger

mkTotNegative :: Coin -> TotalNegative
mkTotNegative = TotalNegative . coinToInteger

mkTotSum :: Coin -> TotalSum
mkTotSum = TotalSum . coinToInteger

-- Proposal is approved (which corresponds to 'Just True') if total
-- stake of votes for it is more than half of total stake.
-- Proposal is rejected (which corresponds to 'Just False') if total
-- stake of votes against it is more than half of total stake.
-- Otherwise proposal is undecided ('Nothing').
isDecided :: TotalPositive -> TotalNegative -> TotalSum -> Maybe Bool
isDecided (TotalPositive totalPositive) (TotalNegative totalNegative) (TotalSum totalSum)
    | totalPositive * 2 > totalSum = Just True
    | totalNegative * 2 > totalSum = Just False
    | otherwise = Nothing

-- | Apply vote to UndecidedProposalState, thus modifing mutable data,
-- i. e. votes and stakes.
voteToUProposalState
    :: MonadError PollVerFailure m
    => PublicKey
    -> Coin
    -> Bool
    -> UndecidedProposalState
    -> m UndecidedProposalState
voteToUProposalState voter stake decision ups@UndecidedProposalState {..} = do
    let upId = hash upsProposal
    -- We need to find out new state of vote (it can be a fresh vote or revote).
    let oldVote = upsVotes ^. at voter
    let oldPositive = maybe False isPositiveVote oldVote
    let oldNegative = maybe False (not . isPositiveVote) oldVote
    let combinedMaybe = decision `combineVotes` oldVote
    combined <-
        note
            (PollExtraRevote
             { perStakeholder = addressHash voter
             , perUpId = upId
             , perDecision = decision
             })
            combinedMaybe
    -- We recalculate new stake taking into account that old vote
    -- could be deactivate.
    let posStakeAfterRemove
            | oldPositive = upsPositiveStake `unsafeSubCoin` stake
            | otherwise = upsPositiveStake
        negStakeAfterRemove
            | oldNegative = upsNegativeStake
            | otherwise = upsNegativeStake `unsafeSubCoin` stake
    -- Then we recalculate stake adding stake of new vote.
        posStakeFinal
            | decision = posStakeAfterRemove `unsafeAddCoin` stake
            | otherwise = posStakeAfterRemove
        negStakeFinal
            | decision = negStakeAfterRemove
            | otherwise = negStakeAfterRemove `unsafeAddCoin` stake
    -- We add a new vote with update state to set of votes.
    let newVotes = HM.insert voter combined upsVotes
    return
        ups
        { upsVotes = newVotes
        , upsPositiveStake = posStakeFinal
        , upsNegativeStake = negStakeFinal
        }

-- Put a new proposal into context of MonadPoll. First argument
-- determines whether proposal is part of existing block or is taken
-- from mempool. State of proposal is calculated from votes for it and
-- their stakes.
putNewProposal
    :: forall ssc m.
       (MonadPoll m)
    => Either SlotId (MainBlockHeader ssc)
    -> Coin
    -> [(UpdateVote, Coin)]
    -> UpdateProposal
    -> m ()
putNewProposal slotOrHeader totalStake votesAndStakes up = addActiveProposal ps
  where
    slotId = either identity (view headerSlot) slotOrHeader
    cd = either (const Nothing) (Just . view difficultyL) slotOrHeader
    totalPositive = sumCoins . map snd . filter (uvDecision . fst) $ votesAndStakes
    totalNegative = sumCoins . map snd . filter (not . uvDecision . fst) $ votesAndStakes
    votes = HM.fromList . map convertVote $ votesAndStakes
    -- New proposal always has a fresh vote (not revote).
    convertVote (UpdateVote {..}, _) = (uvKey, newVoteState uvDecision)
    ups =
        UndecidedProposalState
        { upsVotes = votes
        , upsProposal = up
        , upsSlot = slotId
        , upsPositiveStake = unsafeIntegerToCoin totalPositive
        , upsNegativeStake = unsafeIntegerToCoin totalNegative
        }
    -- New proposal can be in decided state immediately if it has a
    -- lot of positive votes.
    ps
        | Just decision <-
             isDecided
                 (TotalPositive totalPositive)
                 (TotalNegative totalNegative)
                 (mkTotSum totalStake) =
            PSDecided
                DecidedProposalState
                {dpsDecision = decision, dpsUndecided = ups, dpsDifficulty = cd}
    -- Or it can be in undecided state (more common case).
        | otherwise = PSUndecided ups

----------------------------------------------------------------------------
-- Verify and apply
----------------------------------------------------------------------------

-- | Verify UpdatePayload with respect to data provided by
-- MonadPoll. If data is valid it is also applied.  Otherwise
-- PollVerificationFailure is thrown using MonadError type class.
-- When first flag is true and proposal is present,
-- 'updateProposalThreshold' is checked for it, otherwise it's not
-- checked.
-- When second argument is 'Left epoch', it means that temporary payload
-- for given slot is applied.
-- When it is 'Right header', it means that payload from block with
-- given header is applied.
verifyAndApplyUSPayload
    :: (MonadError PollVerFailure m, MonadPoll m)
    => Bool -> Either SlotId (MainBlockHeader __) -> UpdatePayload -> m ()
verifyAndApplyUSPayload considerPropThreshold slotOrHeader UpdatePayload {..} = do
    -- First of all, we split all votes into groups. One group
    -- consists of votes for proposal from payload. Each other group
    -- consists of votes for other proposals.
    let upId = hash <$> upProposal
    let votePredicate vote = maybe False (uvProposalId vote ==) upId
    let (curPropVotes, otherVotes) = partition votePredicate upVotes
    let otherGroups = NE.groupWith uvProposalId otherVotes
    -- When there is proposal in payload, it's verified and applied.
    whenJust
        upProposal
        (verifyAndApplyProposal considerPropThreshold slotOrHeader curPropVotes)
    -- Then we also apply votes from other groups.
    -- ChainDifficulty is needed, because proposal may become approved
    -- and then we'll need to track whether it becomes confirmed.
    let cd = either (const Nothing) (Just . view difficultyL) slotOrHeader
    mapM_ (verifyAndApplyVotesGroup cd) otherGroups
    -- If we are applying payload from block, we also check implicit
    -- agreement rule and depth of decided proposals (they can become
    -- confirmed/discarded).
    case slotOrHeader of
        Left _ -> pass
        Right mainBlk -> do
            applyImplicitAgreement
                (mainBlk ^. headerSlot)
                (mainBlk ^. difficultyL)
            applyDepthCheck (mainBlk ^. difficultyL)

-- Get stake of stakeholder who issued given vote as per given epoch.
-- If stakeholder wasn't richman at that point, PollNotRichman is thrown.
resolveVoteStake
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => EpochIndex -> Coin -> UpdateVote -> m Coin
resolveVoteStake epoch totalStake UpdateVote {..} = do
    let !id = addressHash uvKey
    stake <- note (mkNotRichman id Nothing) =<< getRichmanStake epoch id
    when (stake < threshold) $ throwError $ mkNotRichman id (Just stake)
    return stake
  where
    threshold = applyCoinPortion updateVoteThreshold totalStake
    mkNotRichman id stake =
        PollNotRichman
        {pnrStakeholder = id, pnrThreshold = threshold, pnrStake = stake}

-- Do all necessary checks of new proposal and votes for it.
-- If it's valid, apply. Specifically, these checks are done:
--
-- 1. Check that there is no active proposal for given application.
-- 2. Check script version, it should be consistent with existing
--    script version dependencies. New dependency can be added.
-- 3. Check that numeric software version of application is 1 more than
--    of last confirmed proposal for this application.
-- 4. If 'considerThreshold' is true, also check that sum of positive votes
--    for this proposal is enough (at least 'updateProposalThreshold').
--
-- If all checks pass, proposal is added. It can be in undecided or decided
-- state (if it has enough voted stake at once).
verifyAndApplyProposal
    :: (MonadError PollVerFailure m, MonadPoll m)
    => Bool
    -> Either SlotId (MainBlockHeader __)
    -> [UpdateVote]
    -> UpdateProposal
    -> m ()
verifyAndApplyProposal considerThreshold slotOrHeader votes up@UpdateProposal {..} = do
    let epoch = slotOrHeader ^. epochIndexL
    let !upId = hash up
    -- If there is an active proposal for given application name in
    -- blockchain, new proposal can't be added.
    whenM (hasActiveProposal (svAppName upSoftwareVersion)) $
        throwError $ Poll2ndActiveProposal upSoftwareVersion
    -- Here we verify consistency with regards to script versions and
    -- update relevant state.
    verifyAndApplyProposalScript upId up
    -- We also verify that software version is expected one.
    verifySoftwareVersion upId up
    -- After that we resolve stakes of all votes.
    totalStake <- note (PollUnknownStakes epoch) =<< getEpochTotalStake epoch
    votesAndStakes <-
        mapM (\v -> (v, ) <$> resolveVoteStake epoch totalStake v) votes
    -- When necessary, we also check that proposal itself has enough
    -- positive votes to be included into block.
    when considerThreshold $ verifyProposalStake totalStake votesAndStakes upId
    -- Finally we put it into context of MonadPoll together with votes for it.
    putNewProposal slotOrHeader totalStake votesAndStakes up

-- Here we check that script version from proposal is the same as
-- script versions of other proposals with the same protocol version.
-- We also add new mapping if it is new.
-- Returns True if new script versions deps is created.
verifyAndApplyProposalScript
    :: (MonadError PollVerFailure m, MonadPoll m)
    => UpId -> UpdateProposal -> m ()
verifyAndApplyProposalScript upId UpdateProposal {..} =
    getScriptVersion upProtocolVersion >>= \case
        -- If there is no known script version for given procol
        -- version, it's added.
        Nothing -> addScriptVersionDep upProtocolVersion upScriptVersion
        Just sv
            -- If script version matches stored version, it's good.
            | sv == upScriptVersion -> pass
            -- Otherwise verification fails.
            | otherwise ->
                throwError
                    PollWrongScriptVersion
                    { pwsvExpected = sv
                    , pwsvFound = upScriptVersion
                    , pwsvUpId = upId
                    }

-- Here we check that software version is 1 more than last confirmed
-- version of given application. Or 0 if it's new application.
verifySoftwareVersion
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => UpId -> UpdateProposal -> m ()
verifySoftwareVersion upId UpdateProposal {..} =
    getLastConfirmedSV app >>= \case
        -- If there is no confirmed versions for given application,
        -- We check that version is 0.
        Nothing | svNumber sv == 0 -> pass
                | otherwise ->
                  throwError
                    PollWrongSoftwareVersion
                    { pwsvStored = Nothing
                    , pwsvGiven = svNumber sv
                    , pwsvApp = app
                    , pwsvUpId = upId
                    }
        -- Otherwise we check that version is 1 more than stored
        -- version.
        Just n
            | svNumber sv == n + 1 -> pass
            | otherwise ->
                throwError
                    PollWrongSoftwareVersion
                    { pwsvStored = Just n
                    , pwsvGiven = svNumber sv
                    , pwsvApp = app
                    , pwsvUpId = upId
                    }
  where
    sv = upSoftwareVersion
    app = svAppName sv

-- Here we check that proposal has at least 'updateProposalThreshold'
-- stake of total stake in all positive votes for it.
verifyProposalStake
    :: (MonadError PollVerFailure m)
    => Coin -> [(UpdateVote, Coin)] -> UpId -> m ()
verifyProposalStake totalStake votesAndStakes upId = do
    let threshold = applyCoinPortion updateProposalThreshold totalStake
    let votesSum =
            sumCoins . map snd . filter (uvDecision . fst) $ votesAndStakes
    when (coinToInteger totalStake < votesSum) $
        throwError
            PollSmallProposalStake
            { pspsThreshold = threshold
            , pspsActual = unsafeIntegerToCoin votesSum
            , pspsUpId = upId
            }

-- Here we verify votes for proposal which is already active. Each
-- vote must have enough stake as per distribution from epoch where
-- proposal was added.
-- We also verify that what votes correspond to real proposal in
-- undecided state.
-- Votes are assumed to be for the same proposal.
verifyAndApplyVotesGroup
    :: (MonadError PollVerFailure m, MonadPoll m)
    => Maybe ChainDifficulty -> NonEmpty UpdateVote -> m ()
verifyAndApplyVotesGroup cd votes = do
    let upId = uvProposalId $ NE.head votes
        !stakeholderId = addressHash . uvKey $ NE.head votes
        unknownProposalErr =
            PollUnknownProposal
            {pupStakeholder = stakeholderId, pupProposal = upId}
    ps <- note unknownProposalErr =<< getProposal upId
    case ps of
        PSDecided _     -> throwError $ PollProposalIsDecided upId stakeholderId
        PSUndecided ups -> mapM_ (verifyAndApplyVote cd ups) votes

-- Here we actually apply vote to stored undecided proposal.
verifyAndApplyVote
    :: (MonadError PollVerFailure m, MonadPoll m)
    => Maybe ChainDifficulty -> UndecidedProposalState -> UpdateVote -> m ()
verifyAndApplyVote cd ups v@UpdateVote {..} = do
    let e = siEpoch $ upsSlot ups
    totalStake <- note (PollUnknownStakes e) =<< getEpochTotalStake e
    voteStake <- resolveVoteStake e totalStake v
    newUPS@UndecidedProposalState {..} <-
        voteToUProposalState uvKey voteStake uvDecision ups
    let newPS
            | Just decision <-
                 isDecided
                     (mkTotPositive upsPositiveStake)
                     (mkTotNegative upsNegativeStake)
                     (mkTotSum totalStake) =
                PSDecided
                    DecidedProposalState
                    { dpsUndecided = newUPS
                    , dpsDecision = decision
                    , dpsDifficulty = cd
                    }
            | otherwise = PSUndecided ups
    addActiveProposal newPS

-- According to implicit agreement rule all proposals which were put
-- into blocks earlier than 'updateImplicitApproval' slots before slot
-- of current block become implicitly decided (approved or rejected).
-- If proposal's total positive stake is bigger than negative, it's
-- approved. Otherwise it's rejected.
applyImplicitAgreement
    :: MonadPoll m
    => SlotId -> ChainDifficulty -> m ()
applyImplicitAgreement (flattenSlotId -> slotId) cd
    | slotId < updateImplicitApproval = pass
    | otherwise = do
        let oldSlot = unflattenSlotId $ slotId - updateImplicitApproval
        mapM_ applyImplicitAgreementDo =<< getOldProposals oldSlot
  where
    applyImplicitAgreementDo ups =
        addActiveProposal $ PSDecided $ makeImplicitlyDecided ups
    makeImplicitlyDecided ups@UndecidedProposalState {..} =
        DecidedProposalState
        { dpsUndecided = ups
        , dpsDecision = upsPositiveStake > upsNegativeStake
        , dpsDifficulty = Just cd
        }

-- All decided proposals which became decided more than
-- 'blkSecurityParam' blocks deeper than current block become
-- confirmed or discarded (approved become confirmed, rejected become
-- discarded).
applyDepthCheck
    :: MonadPoll m
    => ChainDifficulty -> m ()
applyDepthCheck cd
    | cd <= blkSecurityParam = pass
    | otherwise = do
        deepProposals <- getDeepProposals (cd - blkSecurityParam)
        mapM_ applyDepthCheckDo deepProposals
  where
    applyDepthCheckDo DecidedProposalState {..} = do
        let UndecidedProposalState {..} = dpsUndecided
        let sv = upSoftwareVersion upsProposal
        when dpsDecision $ setLastConfirmedSV sv
        deactivateProposal (hash upsProposal)

----------------------------------------------------------------------------
-- Rollback
----------------------------------------------------------------------------

-- | Rollback application of UpdatePayload in MonadPoll using payload
-- itself and undo data.
rollbackUSPayload
    :: forall m . MonadPoll m
    => ChainDifficulty -> UpdatePayload -> USUndo -> m ()
rollbackUSPayload _ UpdatePayload{..} USUndo{..} = do
    -- Rollback last confirmed
    mapM_ setOrDelLastConfirmedSV (HM.toList unChangedSV)
    -- Rollback proposals
    mapM_ setOrDelProposal (HM.toList unChangedProps)
    -- Rollback protocol version
    whenJust unLastAdoptedPV setLastAdoptedPV
    -- Rollback script
    whenJust unCreatedNewDepsFor delScriptVersionDep
  where
    setOrDelLastConfirmedSV :: (ApplicationName, PrevValue NumSoftwareVersion) -> m ()
    setOrDelLastConfirmedSV (svAppName, PrevValue svNumber) =
        setLastConfirmedSV SoftwareVersion {..}
    setOrDelLastConfirmedSV (appName, NoExist) = delConfirmedSV appName

    setOrDelProposal :: (UpId, PrevValue ProposalState) -> m ()
    setOrDelProposal (upid, NoExist)   = deactivateProposal upid
    setOrDelProposal (_, PrevValue ps) = addActiveProposal ps

----------------------------------------------------------------------------
-- Normalize
----------------------------------------------------------------------------

-- | Remove some data from Poll to make it valid. First argument
-- determines whether 'updateProposalThreshold' should be checked.
normalizePoll
    :: MonadPoll m
    => Bool -> m ()
normalizePoll _ = const pass notImplemented
