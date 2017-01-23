{-# LANGUAGE Rank2Types #-}

-- | Base operations in Poll.

module Pos.Update.Poll.Logic.Base
       ( TotalPositive (..)
       , TotalNegative (..)
       , TotalSum (..)
       , mkTotPositive
       , mkTotNegative
       , mkTotSum

       , canCreateBlockBV
       , isConfirmedBV
       , getBVScript
       , confirmBlockVersion

       , isDecided
       , voteToUProposalState
       , putNewProposal
       ) where

import           Control.Lens          (at)
import           Control.Monad.Except  (MonadError)
import qualified Data.HashMap.Strict   as HM
import           Universum

import           Pos.Crypto            (PublicKey, hash)
import           Pos.Script.Type       (ScriptVersion)
import           Pos.Types             (BlockVersion (..), Coin, MainBlockHeader, SlotId,
                                        addressHash, coinToInteger, difficultyL,
                                        headerSlot, sumCoins, unsafeAddCoin,
                                        unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Update.Core       (UpdateProposal (..), UpdateVote (..),
                                        combineVotes, isPositiveVote, newVoteState)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types (BlockVersionState (..), DecidedProposalState (..),
                                        PollVerFailure (..), ProposalState (..),
                                        UndecidedProposalState (..))

----------------------------------------------------------------------------
-- Wrappers for type-safety
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

----------------------------------------------------------------------------
-- Basic operations
----------------------------------------------------------------------------

-- | Check whether BlockVersion is confirmed.
isConfirmedBV :: MonadPollRead m => BlockVersion -> m Bool
isConfirmedBV = fmap (maybe False bvsIsConfirmed) . getBVState

-- | Get 'ScriptVersion' associated with given 'BlockVersion' if it is known.
getBVScript :: MonadPollRead m => BlockVersion -> m (Maybe ScriptVersion)
getBVScript = fmap (maybe Nothing (Just . bvsScript)) . getBVState

-- | Mark given 'BlockVersion' as confirmed if it is known.
confirmBlockVersion :: MonadPoll m => BlockVersion -> m ()
confirmBlockVersion bv =
    getBVState bv >>= \case
        Nothing -> pass
        Just bvs -> putBVState bv bvs {bvsIsConfirmed = True}

-- | Check whether block with given 'BlockVersion' can be created
-- according to current Poll.
--
-- Specifically, one of the following conditions must be true.
-- • Given block version is equal to last adopted block version.
-- • '(major, minor)' from given block version must be greater than
-- '(major, minor)' if last adopted version and this block version must be
-- confirmed.
canCreateBlockBV :: MonadPollRead m => BlockVersion -> m Bool
canCreateBlockBV bv = do
    lastAdopted <- getLastAdoptedBV
    isConfirmed <- isConfirmedBV bv
    let toMajMin BlockVersion {..} = (bvMajor, bvMinor)
    return
        (bv == lastAdopted ||
         (toMajMin bv > toMajMin lastAdopted && isConfirmed))

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
            | oldNegative = upsNegativeStake `unsafeSubCoin` stake
            | otherwise = upsNegativeStake
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
