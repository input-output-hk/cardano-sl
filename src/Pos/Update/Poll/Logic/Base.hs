{-# LANGUAGE Rank2Types #-}

-- | Base operations in Poll.

module Pos.Update.Poll.Logic.Base
       ( TotalPositive (..)
       , TotalNegative (..)
       , TotalSum (..)
       , mkTotPositive
       , mkTotNegative
       , mkTotSum

       , adoptBlockVersion
       , canBeAdoptedBV
       , canBeProposedBV
       , canCreateBlockBV
       , isConfirmedBV
       , getBVScriptVersion
       , confirmBlockVersion

       , isDecided
       , voteToUProposalState
       , putNewProposal
       ) where

import           Control.Lens          (at)
import           Control.Monad.Except  (MonadError)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Set              as S
import           Universum

import           Pos.Crypto            (PublicKey, hash)
import           Pos.Script.Type       (ScriptVersion)
import           Pos.Ssc.Class         (Ssc)
import           Pos.Types             (BlockVersion (..), Coin, HeaderHash,
                                        MainBlockHeader, SlotId, addressHash,
                                        coinToInteger, difficultyL, headerHashG,
                                        headerSlot, sumCoins, unsafeAddCoin,
                                        unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Update.Core       (UpdateProposal (..), UpdateVote (..),
                                        combineVotes, isPositiveVote, newVoteState)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types (BlockVersionState (..),
                                        ConfirmedProposalState (..),
                                        DecidedProposalState (..), DpsExtra (..),
                                        PollVerFailure (..), ProposalState (..),
                                        UndecidedProposalState (..), UpsExtra (..),
                                        cpsBlockVersion)

----------------------------------------------------------------------------
-- BlockVersion-related simple functions/operations
----------------------------------------------------------------------------

-- | Check whether BlockVersion is confirmed.
isConfirmedBV :: MonadPollRead m => BlockVersion -> m Bool
isConfirmedBV = fmap (maybe False bvsIsConfirmed) . getBVState

-- | Get 'ScriptVersion' associated with given 'BlockVersion' if it is known.
getBVScriptVersion :: MonadPollRead m => BlockVersion -> m (Maybe ScriptVersion)
getBVScriptVersion = fmap (fmap bvsScriptVersion) . getBVState

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

-- | Check whether given 'BlockVersion' can be proposed according to
-- current Poll.
--
-- Specifically, the following rules regarding major and minor versions
-- take place:
-- 1. If major version is less than last adopted one, it can't be proposed.
-- 2. If major version is more than '1' greater than last adopted one,
-- it can't be proposed as well.
-- 3. If major version is greater than last adopted one by '1', then minor
-- version must be '0'.
-- 4. If major version is equal to the last adopted one, then minor version
-- can be either same as the last adopted one or greater by '1'.
-- Rules regarding alternative version are as follows (assuming
-- checks above pass):
-- 1. If '(Major, Minor)' of given version is equal to '(Major, Minor)' of
-- last adopted version, then alternative version must be equal to
-- alternative version of last adopted version.
-- 2. Otherwise '(Major, Minor)' of given version is lexicographically greater
-- than or equal to '(Major, Minor)' of last adopted version and in this case
-- other proposed block versions with same '(Major, Minor)' are considered
-- (let's call this set 'X').
-- If 'X' is empty, given alternative version must be 0.
-- Otherwise it must be in 'X' or greater than maximum from 'X' by one.
canBeProposedBV :: MonadPollRead m => BlockVersion -> m Bool
canBeProposedBV bv =
    canBeProposedPure bv <$> getLastAdoptedBV <*>
    (S.fromList <$> getProposedBVs)

canBeProposedPure :: BlockVersion -> BlockVersion -> Set BlockVersion -> Bool
canBeProposedPure BlockVersion { bvMajor = givenMajor
                               , bvMinor = givenMinor
                               , bvAlt = givenAlt
                               } BlockVersion { bvMajor = adoptedMajor
                                              , bvMinor = adoptedMinor
                                              , bvAlt = adoptedAlt
                                              } proposed
    | givenMajor < adoptedMajor = False
    | givenMajor > adoptedMajor + 1 = False
    | givenMajor == adoptedMajor + 1 && givenMinor /= 0 = False
    | givenMajor == adoptedMajor &&
          givenMinor /= adoptedMinor && givenMinor /= adoptedMinor + 1 = False
    | (givenMajor, givenMinor) == (adoptedMajor, adoptedMinor) =
        givenAlt == adoptedAlt
    -- At this point we know that
    -- '(givenMajor, givenMinor) > (adoptedMajor, adoptedMinor)'
    | null relevantProposed = givenAlt == 0
    | otherwise =
        givenAlt == (S.findMax relevantProposed + 1) ||
        givenAlt `S.member` relevantProposed
  where
    -- Here we can use mapMonotonic, even though 'bvAlt' itself is not
    -- necessary monotonic.
    -- That's because after filtering all versions have same major and minor
    -- components.
    relevantProposed = S.mapMonotonic bvAlt $ S.filter predicate proposed
    predicate BlockVersion {..} = bvMajor == givenMajor && bvMinor == givenMinor

-- | Check whether given 'BlockVersion' can be adopted according to
-- current Poll.
--
-- Specifically, the following rules regarding major and minor versions
-- take place:
-- 1. If major version is less than last adopted one, it can't be adopted.
-- 2. If major version is more than '1' greater than last adopted one,
-- it can't be adopted as well.
-- 3. If major version is greater than last adopted one by '1', then minor
-- version must be '0'.
-- 4. If major version is equal to the last adopted one, then minor version
-- can be greather than minor component of last adopted version by 1.
canBeAdoptedBV :: MonadPollRead m => BlockVersion -> m Bool
canBeAdoptedBV bv = canBeAdoptedPure bv <$> getLastAdoptedBV

canBeAdoptedPure :: BlockVersion -> BlockVersion -> Bool
canBeAdoptedPure BlockVersion { bvMajor = givenMajor
                              , bvMinor = givenMinor
                              , bvAlt = givenAlt
                              }
                 BlockVersion { bvMajor = adoptedMajor
                              , bvMinor = adoptedMinor
                              , bvAlt = adoptedAlt
                              }
    | givenMajor < adoptedMajor = False
    | givenMajor > adoptedMajor + 1 = False
    | givenMajor == adoptedMajor + 1 = givenMinor == 0
    | givenMajor == adoptedMajor = givenMinor == adoptedMinor + 1

-- | Adopt given block version. When it happens, last adopted block
-- version is changed.
--
-- Apart from that, 'ConfirmedProposalState' of proposals with this
-- block version are updated.
adoptBlockVersion
    :: MonadPoll m
    => HeaderHash -> BlockVersion -> m ()
adoptBlockVersion winningBlk bv = do
    setLastAdoptedBV bv
    mapM_ processConfirmed =<< getConfirmedProposals
  where
    processConfirmed cps
        | cpsBlockVersion cps /= bv = pass
        | otherwise = addConfirmedProposal cps {cpsAdopted = Just winningBlk}

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
       (MonadPoll m, Ssc ssc)
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
    blkHeaderHash = either (const Nothing) (Just . view headerHashG) slotOrHeader
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
        , upsExtra = UpsExtra <$> blkHeaderHash
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
                { dpsDecision = decision
                , dpsUndecided = ups
                , dpsDifficulty = cd
                , dpsExtra = DpsExtra <$> blkHeaderHash <*> Just False
                }
    -- Or it can be in undecided state (more common case).
        | otherwise = PSUndecided ups
