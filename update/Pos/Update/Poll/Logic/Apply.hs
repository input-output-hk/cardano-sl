-- | Logic of application and verification of data in Poll.

module Pos.Update.Poll.Logic.Apply
       ( verifyAndApplyUSPayload
       , verifyAndApplyProposal
       , verifyAndApplyVoteDo
       ) where

import           Control.Monad.Except (MonadError, throwError)
import qualified Data.HashSet as HS
import           Data.List (partition)
import qualified Data.List.NonEmpty as NE
import           Formatting (build, builder, int, sformat, (%))
import           System.Wlog (logDebug, logInfo, logNotice)
import           Universum

import           Pos.Binary.Class (biSize)
import           Pos.Core (ChainDifficulty (..), Coin, EpochIndex, HeaderHash, IsMainHeader (..),
                           SlotId (siEpoch), SoftwareVersion (..), addressHash, applyCoinPortionUp,
                           blockVersionL, coinToInteger, difficultyL, epochIndexL, flattenSlotId,
                           headerHashG, headerSlotL, sumCoins, unflattenSlotId, unsafeIntegerToCoin)
import           Pos.Core.Configuration (HasConfiguration, blkSecurityParam)
import           Pos.Core.Update (BlockVersion, BlockVersionData (..), UpId, UpdatePayload (..),
                                  UpdateProposal (..), UpdateVote (..), bvdUpdateProposalThd)
import           Pos.Crypto (hash, shortHashF)
import           Pos.Data.Attributes (areAttributesKnown)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Failure (PollVerFailure (..))
import           Pos.Update.Poll.Logic.Base (canBeAdoptedBV, canCreateBlockBV, confirmBlockVersion,
                                             isDecided, mkTotNegative, mkTotPositive, mkTotSum,
                                             putNewProposal, voteToUProposalState)
import           Pos.Update.Poll.Logic.Version (verifyAndApplyProposalBVS, verifyBlockVersion,
                                                verifySoftwareVersion)
import           Pos.Update.Poll.Types (ConfirmedProposalState (..), DecidedProposalState (..),
                                        DpsExtra (..), ProposalState (..),
                                        UndecidedProposalState (..), UpsExtra (..), psProposal)
import           Pos.Util.Some (Some (..))

type ApplyMode m =
    ( MonadError PollVerFailure m
    , MonadPoll m
    , HasConfiguration
    )

-- | Verify UpdatePayload with respect to data provided by
-- MonadPoll. If data is valid it is also applied.  Otherwise
-- PollVerificationFailure is thrown using MonadError type class.
--
-- The first argument specifies whether we should perform unknown data
-- checks. Currently it means that if it's 'True', then proposal (if
-- it exists) must not have unknown attributes.
--
-- When the second argument is 'Left epoch', it means that temporary payload
-- for given slot is applied. In this case threshold for inclusion of proposal
-- into block is intentionally not checked.
-- When it is 'Right header', it means that payload from block with
-- given header is applied and in this case threshold for update proposal is
-- checked.
verifyAndApplyUSPayload ::
       ApplyMode m
    => BlockVersion
    -> Bool
    -> Either SlotId (Some IsMainHeader)
    -> UpdatePayload
    -> m ()
verifyAndApplyUSPayload lastAdopted verifyAllIsKnown slotOrHeader UpdatePayload {..} = do
    -- We first check header, then payload, then we do implicit checks.
    whenRight slotOrHeader $ verifyHeader lastAdopted
    unless isEmptyPayload $ do
        -- Then we split all votes into groups. One group consists of
        -- votes for proposal from payload. Each other group consists of
        -- votes for other proposals.
        let upId = hash <$> upProposal
        let votePredicate vote = maybe False (uvProposalId vote ==) upId
        let (curPropVotes, otherVotes) = partition votePredicate upVotes
        let otherGroups = NE.groupWith uvProposalId otherVotes
        -- When there is proposal in payload, it's verified and applied.
        whenJust upProposal $
            verifyAndApplyProposal verifyAllIsKnown slotOrHeader curPropVotes
        -- Then we also apply votes from other groups.
        -- ChainDifficulty is needed, because proposal may become approved
        -- and then we'll need to track whether it becomes confirmed.
        let cd = case slotOrHeader of
                Left  _ -> Nothing
                Right h -> Just (h ^. difficultyL, h ^. headerHashG)
        mapM_ (verifyAndApplyVotesGroup cd) otherGroups
    -- If we are applying payload from block, we also check implicit
    -- agreement rule and depth of decided proposals (they can become
    -- confirmed/discarded).
    case slotOrHeader of
        Left _           -> pass
        Right mainHeader -> do
            applyImplicitAgreement
                (mainHeader ^. headerSlotL)
                (mainHeader ^. difficultyL)
                (mainHeader ^. headerHashG)
            applyDepthCheck
                (mainHeader ^. epochIndexL)
                (mainHeader ^. headerHashG)
                (mainHeader ^. difficultyL)
  where
    isEmptyPayload = isNothing upProposal && null upVotes

-- Here we verify all US-related data from header.
verifyHeader
    :: (MonadError PollVerFailure m, MonadPoll m, IsMainHeader mainHeader)
    => BlockVersion -> mainHeader -> m ()
verifyHeader lastAdopted header = do
    let versionInHeader = header ^. blockVersionL
    unlessM (canCreateBlockBV lastAdopted versionInHeader) $ do
        throwError
            PollWrongHeaderBlockVersion
            {pwhpvGiven = versionInHeader, pwhpvAdopted = lastAdopted}

-- Get stake of stakeholder who issued given vote as per given epoch.
-- If stakeholder wasn't richman at that point, PollNotRichman is thrown.
resolveVoteStake
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => EpochIndex -> Coin -> UpdateVote -> m Coin
resolveVoteStake epoch totalStake vote = do
    let !id = addressHash (uvKey vote)
    thresholdPortion <- bvdUpdateProposalThd <$> getAdoptedBVData
    let threshold = applyCoinPortionUp thresholdPortion totalStake
    let errNotRichman mbStake = PollNotRichman
            { pnrStakeholder = id
            , pnrThreshold   = threshold
            , pnrStake       = mbStake }
    stake <- note (errNotRichman Nothing) =<< getRichmanStake epoch id
    when (stake < threshold) $
        throwError $ errNotRichman (Just stake)
    return stake

-- Do all necessary checks of new proposal and votes for it.
-- If it's valid, apply. Specifically, these checks are done:
--
-- 1. Check that no one stakeholder sent two update proposals within current epoch.
-- 2. If 'verifyAllIsKnown' is 'True', check that proposal has
--    no unknown attributes.
-- 3. Proposal must not exceed maximal proposal size.
-- 4. Check that there is no active proposal with the same id.
-- 5. Verify consistenty with BlockVersionState for protocol version from proposal.
-- 6. Verify that protocol version from proposal can follow last adopted protocol version.
-- 7. Check that numeric software version of application is 1 more than
--    of last confirmed proposal for this application.
-- 8. If 'slotOrHeader' is 'Right', also check that sum of positive votes
--    for this proposal is enough (at least 'updateProposalThd').
--
-- If all checks pass, proposal is added. It can be in undecided or decided
-- state (if it has enough voted stake at once).
verifyAndApplyProposal
    :: (HasConfiguration, MonadError PollVerFailure m, MonadPoll m)
    => Bool
    -> Either SlotId (Some IsMainHeader)
    -> [UpdateVote]
    -> UpdateProposal
    -> m ()
verifyAndApplyProposal verifyAllIsKnown slotOrHeader votes
                           up@UncheckedUpdateProposal {..} = do
    let !upId = hash up
    let !upFromId = addressHash upFrom
    whenM (HS.member upFromId <$> getEpochProposers) $
        throwError $ PollMoreThanOneProposalPerEpoch upFromId upId
    let epoch = slotOrHeader ^. epochIndexL
    let proposalSize = biSize up
    proposalSizeLimit <- bvdMaxProposalSize <$> getAdoptedBVData
    when (verifyAllIsKnown && not (areAttributesKnown upAttributes)) $
        throwError $
        PollUnknownAttributesInProposal
        { puapUpId = upId
        , puapAttrs = upAttributes
        }
    when (proposalSize > proposalSizeLimit) $
        throwError $
        PollTooLargeProposal
        { ptlpUpId = upId
        , ptlpSize = proposalSize
        , ptlpLimit = proposalSizeLimit
        }
    whenJustM (getProposal upId) $
        const $ throwError $ PollProposalAlreadyActive upId
    -- Here we verify consistency with regards to data from 'BlockVersionState'
    -- and update relevant state if necessary.
    verifyAndApplyProposalBVS upId epoch up
    -- Then we verify that protocol version from proposal can follow last
    -- adopted protocol version.
    verifyBlockVersion upId up
    -- We also verify that software version is expected one.
    verifySoftwareVersion upId up
    -- After that we resolve stakes of all votes.
    totalStake <- note (PollUnknownStakes epoch) =<< getEpochTotalStake epoch
    votesAndStakes <-
        mapM (\v -> (v, ) <$> resolveVoteStake epoch totalStake v) votes
    -- When necessary, we also check that proposal itself has enough
    -- positive votes to be included into block.
    when (isRight slotOrHeader) $
        verifyProposalStake totalStake votesAndStakes upId
    -- Finally we put it into context of MonadPoll together with votes for it.
    putNewProposal slotOrHeader totalStake votesAndStakes up

-- Here we check that proposal has at least 'bvdUpdateProposalThd' stake of
-- total stake in all positive votes for it.
verifyProposalStake
    :: (MonadPollRead m, MonadError PollVerFailure m)
    => Coin -> [(UpdateVote, Coin)] -> UpId -> m ()
verifyProposalStake totalStake votesAndStakes upId = do
    thresholdPortion <- bvdUpdateProposalThd <$> getAdoptedBVData
    let threshold = applyCoinPortionUp thresholdPortion totalStake
    let thresholdInt = coinToInteger threshold
    let votesSum =
            sumCoins . map snd . filter (uvDecision . fst) $ votesAndStakes
    logDebug $
        sformat
            ("Verifying stake for proposal "%shortHashF%
             ", threshold is "%int%", voted stake is "%int)
            upId thresholdInt votesSum
    when (votesSum < thresholdInt) $
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
    :: ApplyMode m
    => Maybe (ChainDifficulty, HeaderHash) -> NonEmpty UpdateVote -> m ()
verifyAndApplyVotesGroup cd votes = mapM_ verifyAndApplyVote votes
  where
    upId = uvProposalId $ NE.head votes
    verifyAndApplyVote vote = do
        let !stakeholderId = addressHash . uvKey $ NE.head votes
            unknownProposalErr =
                PollUnknownProposal
                {pupStakeholder = stakeholderId, pupProposal = upId}
        ps <- note unknownProposalErr =<< getProposal upId
        case ps of
            PSDecided _     -> throwError $ PollProposalIsDecided upId stakeholderId
            PSUndecided ups -> verifyAndApplyVoteDo cd ups vote

-- Here we actually apply vote to stored undecided proposal.
verifyAndApplyVoteDo
    :: ApplyMode m
    => Maybe (ChainDifficulty, HeaderHash)
    -> UndecidedProposalState
    -> UpdateVote
    -> m ()
verifyAndApplyVoteDo cd ups vote = do
    let e = siEpoch $ upsSlot ups
    totalStake <- note (PollUnknownStakes e) =<< getEpochTotalStake e
    voteStake <- resolveVoteStake e totalStake vote
    newUPS@UndecidedProposalState {..} <-
        voteToUProposalState (uvKey vote) voteStake (uvDecision vote) ups
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
                    , dpsDifficulty = fst <$> cd
                    , dpsExtra = DpsExtra . snd <$> cd <*> Just False
                    }
            | otherwise = PSUndecided newUPS
    insertActiveProposal newPS

-- According to implicit agreement rule all proposals which were put
-- into blocks earlier than 'updateImplicit' slots before slot
-- of current block become implicitly decided (approved or rejected).
-- If proposal's total positive stake is bigger than negative, it's
-- approved. Otherwise it's rejected.
applyImplicitAgreement
    :: (HasConfiguration, MonadPoll m)
    => SlotId -> ChainDifficulty -> HeaderHash -> m ()
applyImplicitAgreement (flattenSlotId -> slotId) cd hh = do
    BlockVersionData {..} <- getAdoptedBVData
    let oldSlot = unflattenSlotId $ slotId - bvdUpdateImplicit
    -- There is no one implicit agreed proposal
    -- when slot of block is less than @bvdUpdateImplicit@
    unless (slotId < bvdUpdateImplicit) $
        mapM_ applyImplicitAgreementDo =<< getOldProposals oldSlot
  where
    applyImplicitAgreementDo ups = do
        let decided = makeImplicitlyDecided ups
        insertActiveProposal $ PSDecided decided
        let upId = hash $ upsProposal ups
            status | dpsDecision decided = "approved"
                   | otherwise = "rejected"
        logInfo $ sformat ("Proposal "%build%" is implicitly "%builder)
            upId status
    makeImplicitlyDecided ups@UndecidedProposalState {..} =
        DecidedProposalState
        { dpsUndecided = ups
        , dpsDecision = upsPositiveStake > upsNegativeStake
        , dpsDifficulty = Just cd
        , dpsExtra = Just $ DpsExtra hh True
        }

-- All decided proposals which became decided more than
-- 'blkSecurityParam' blocks deeper than current block become
-- confirmed or discarded (approved become confirmed, rejected become
-- discarded).
applyDepthCheck
    :: forall m . ApplyMode m
    => EpochIndex -> HeaderHash -> ChainDifficulty -> m ()
applyDepthCheck epoch hh (ChainDifficulty cd)
    | cd <= blkSecurityParam = pass
    | otherwise = do
        deepProposals <- getDeepProposals (ChainDifficulty (cd - blkSecurityParam))
        -- 1. Group proposals by application name
        -- 2. Sort proposals in each group by tuple
        --     (decision, whether decision is implicit, positive stake, slot when it has been proposed)
        -- 3. We discard all proposals in each group except the head
        -- 4. Concatenate all groups and process all proposals
        let winners =
                concatMap (toList . discardAllExceptHead . NE.sortBy proposalCmp) $
                NE.groupWith groupCriterion deepProposals
        unless (null deepProposals) $ mapM_ applyDepthCheckDo winners
  where
    upsAppName = svAppName . upSoftwareVersion . upsProposal
    discardAllExceptHead (a:|xs) = a :| map (\x->x {dpsDecision = False}) xs
    groupCriterion = upsAppName . dpsUndecided
    mkTuple a extra =
        ( dpsDecision a
        , not $ deImplicit extra
        , upsPositiveStake $ dpsUndecided a
        , upsSlot $ dpsUndecided a
        )

    -- This comparator chooses the most appropriate proposal among
    -- proposals of one app and with same chain difficulty.
    proposalCmp a b
      | Just extraA <- dpsExtra a
      , Just extraB <- dpsExtra b =
          compare (mkTuple b extraB) (mkTuple a extraA)
      -- The following checks just in case,
      -- if there are proposals without dpsExtra
      | Just _ <- dpsExtra a = LT
      | Just _ <- dpsExtra b = GT
      | otherwise =
          compare  (upsSlot $ dpsUndecided b) (upsSlot $ dpsUndecided a)

    applyDepthCheckDo :: DecidedProposalState -> m ()
    applyDepthCheckDo DecidedProposalState {..} = do
        let UndecidedProposalState {..} = dpsUndecided
        let sv = upSoftwareVersion upsProposal
        let bv = upBlockVersion upsProposal
        let upId = hash upsProposal
        let status | dpsDecision = "confirmed"
                   | otherwise = "discarded"
        when dpsDecision $ do
            setLastConfirmedSV sv
            DpsExtra {..} <-
                note (PollInternalError "DPS extra: expected Just, but got Nothing")
                      dpsExtra
            UpsExtra {..} <-
                note (PollInternalError "UPS extra: expected Just, but got Nothing")
                      upsExtra
            let cps = ConfirmedProposalState
                    { cpsUpdateProposal = upsProposal
                    , cpsVotes = upsVotes
                    , cpsPositiveStake = upsPositiveStake
                    , cpsNegativeStake = upsNegativeStake
                    , cpsImplicit = deImplicit
                    , cpsProposed = ueProposedBlk
                    , cpsDecided = deDecidedBlk
                    , cpsConfirmed = hh
                    , cpsAdopted = Nothing
                    }
            addConfirmedProposal cps
            proposals <- getProposalsByApp $ upsAppName dpsUndecided
            mapM_ (deactivateProposal . hash . psProposal) proposals
        needConfirmBV <- (dpsDecision &&) <$> canBeAdoptedBV bv
        if | needConfirmBV -> do
               confirmBlockVersion epoch bv
               logInfo $ sformat (build%" is competing now") bv
           | otherwise -> do
               delBVState bv
               logInfo $ sformat ("State of "%build%" is deleted") bv
        deactivateProposal upId
        logNotice $ sformat ("Proposal "%shortHashF%" is "%builder) upId status
