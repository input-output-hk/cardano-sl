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
       , isCompetingBV
       , confirmBlockVersion
       , updateSlottingData
       , verifyNextBVMod
       , CurEpoch
       , ConfirmedEpoch
       , calcSoftforkThreshold

       , isDecided
       , voteToUProposalState
       , putNewProposal
       ) where

import           Universum

import           Control.Lens (at)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import           Data.Tagged (Tagged, untag)
import           Data.Time.Units (convertUnit)
import           Formatting (build, int, sformat, (%))
import           System.Wlog (WithLogger, logDebug, logNotice)

import           Pos.Binary.Update ()
import           Pos.Core (BlockVersion (..), Coin, EpochIndex, HeaderHash, HasProtocolConstants,
                           IsMainHeader (..), SlotId, SoftforkRule (..), TimeDiff (..), addressHash,
                           applyCoinPortionUp, coinPortionDenominator, coinToInteger, difficultyL,
                           epochSlots, getCoinPortion, headerHashG, isBootstrapEra, CoinPortion (..),
                           sumCoins, unsafeAddCoin, unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Core.Update (BlockVersionData (..), BlockVersionModifier (..), UpId,
                                  UpdateProposal (..), UpdateVote (..))
import           Pos.Crypto (PublicKey, hash, shortHashF)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData, addEpochSlottingData,
                                     getCurrentEpochIndex, getNextEpochSlottingData)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Failure (PollVerFailure (..))
import           Pos.Update.Poll.Types (BlockVersionState (..), ConfirmedProposalState (..),
                                        DecidedProposalState (..), DpsExtra (..),
                                        ProposalState (..), UndecidedProposalState (..),
                                        UpsExtra (..), bvsIsConfirmed, combineVotes,
                                        cpsBlockVersion, isPositiveVote, newVoteState)



----------------------------------------------------------------------------
-- BlockVersion-related simple functions/operations
----------------------------------------------------------------------------

-- | Check whether BlockVersion is competing.
isCompetingBV :: MonadPollRead m => BlockVersion -> m Bool
isCompetingBV = fmap (maybe False bvsIsConfirmed) . getBVState

-- | Mark given 'BlockVersion' as confirmed if it is known. This
-- function also takes epoch when proposal was confirmed.
confirmBlockVersion :: MonadPoll m => EpochIndex -> BlockVersion -> m ()
confirmBlockVersion confirmedEpoch bv =
    getBVState bv >>= \case
        Nothing  -> pass
        Just bvs -> putBVState bv bvs {bvsConfirmedEpoch = Just confirmedEpoch}

-- | Check whether block with given 'BlockVersion' can be created
-- according to current Poll.
--
-- Specifically, one of the following conditions must be true.
-- • Given block version is equal to last adopted block version.
-- • Given block version is competing
canCreateBlockBV :: MonadPollRead m => BlockVersion -> BlockVersion -> m Bool
canCreateBlockBV lastAdopted bv = do
    isCompeting <- isCompetingBV bv
    pure (bv == lastAdopted || isCompeting)


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
-- than '(Major, Minor)' of last adopted version and in this case
-- other proposed block versions with same '(Major, Minor)' are considered
-- (let's call this set 'X').
-- If 'X' is empty, given alternative version must be 0.
-- Otherwise it must be in 'X' or greater than maximum from 'X' by one.
canBeProposedBV :: MonadPollRead m => BlockVersion -> m Bool
canBeProposedBV bv =
    canBeProposedPure bv <$> getAdoptedBV <*>
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
canBeAdoptedBV bv = canBeAdoptedPure bv <$> getAdoptedBV

canBeAdoptedPure :: BlockVersion -> BlockVersion -> Bool
canBeAdoptedPure BlockVersion { bvMajor = givenMajor
                              , bvMinor = givenMinor
                              }
                 BlockVersion { bvMajor = adoptedMajor
                              , bvMinor = adoptedMinor
                              }
    | givenMajor < adoptedMajor = False
    | givenMajor > adoptedMajor + 1 = False
    | givenMajor == adoptedMajor + 1 = givenMinor == 0
    | otherwise = givenMinor == adoptedMinor + 1

-- | Adopt given block version. When it happens, last adopted block
-- version is changed.
--
-- Apart from that, 'ConfirmedProposalState' of proposals with this
-- block version are updated.
adoptBlockVersion
    :: MonadPoll m
    => HeaderHash -> BlockVersion -> m ()
adoptBlockVersion winningBlk bv = do
    setAdoptedBV bv
    logNotice $ sformat logFmt bv winningBlk
    mapM_ processConfirmed =<< getConfirmedProposals
  where
    processConfirmed cps
        | cpsBlockVersion cps /= bv = pass
        | otherwise = addConfirmedProposal cps {cpsAdopted = Just winningBlk}
    logFmt = "BlockVersion is adopted: "%build%"; winning block was "%shortHashF

-- | Update slotting data stored in poll. First argument is epoch for
-- which currently adopted 'BlockVersion' can be applied. Here we update the
-- @SlottingData@ from the update. We can recieve updated epoch @SlottingData@
-- and from it, changed epoch/slot times, which is important to keep track of.
updateSlottingData
    :: (HasProtocolConstants, MonadError PollVerFailure m, MonadPoll m)
    => EpochIndex
    -> m ()
updateSlottingData epochIndex = do
    let errFmt =
            ("can't update slotting data, stored current epoch is "%int%
             ", while given epoch is "%int%
             ")")

    slottingData         <- getSlottingData

    let currentEpochIndex = getCurrentEpochIndex slottingData
    let nextEpochSD       = getNextEpochSlottingData slottingData

    if | currentEpochIndex + 1 == epochIndex ->
          -- We don't need an epochIndex since it's always being added at the end.
          updateSlottingDataDo slottingData nextEpochSD
       -- This can happen if there was rollback of genesis block.
       | currentEpochIndex == epochIndex -> pass
       | otherwise ->
           throwError $ PollInternalError $ sformat errFmt currentEpochIndex epochIndex
  where
    -- | Here we calculate the new @EpochSlottingData@ and add it in the
    -- @MonadPoll@ memory @SlottingData@.
    updateSlottingDataDo :: MonadPoll m => SlottingData -> EpochSlottingData -> m ()
    updateSlottingDataDo slottingData esd = do
        latestSlotDuration <- bvdSlotDuration <$> getAdoptedBVData

        let epochDuration = fromIntegral epochSlots * convertUnit (esdSlotDuration esd)
        let newLastStartDiff = esdStartDiff esd + TimeDiff epochDuration
        let newESD = EpochSlottingData {
              esdSlotDuration = latestSlotDuration
            , esdStartDiff    = newLastStartDiff
            }

        let newSlottingData = addEpochSlottingData slottingData newESD

        setSlottingData newSlottingData

-- | Verify that 'BlockVersionModifier' passed as last argument can follow
-- 'BlockVersionData' passed as second argument. First argument
-- ('UpId') is used to create error only.
verifyNextBVMod
    :: MonadError PollVerFailure m
    => UpId
    -> EpochIndex -- block epoch index
    -> BlockVersionData
    -> BlockVersionModifier
    -> m ()
verifyNextBVMod upId epoch
  BlockVersionData { bvdScriptVersion = oldSV
                   , bvdMaxBlockSize = oldMBS
                   , bvdUnlockStakeEpoch = oldUnlockStakeEpoch
                   }
  BlockVersionModifier { bvmScriptVersion = newSVM
                       , bvmMaxBlockSize = newMBSM
                       , bvmUnlockStakeEpoch = newUnlockStakeEpochM
                       }
    | Just newSV <- newSVM,
      newSV /= oldSV + 1 && newSV /= oldSV =
        throwError
            PollWrongScriptVersion
            { pwsvAdopted = oldSV
            , pwsvProposed = newSV
            , pwsvUpId = upId
            }
    | Just newMBS <- newMBSM,
      newMBS > oldMBS * 2 =
        throwError
            PollLargeMaxBlockSize
            { plmbsMaxPossible = oldMBS * 2
            , plmbsFound = newMBS
            , plmbsUpId = upId
            }
    | Just newUnlockStakeEpoch <- newUnlockStakeEpochM,
      oldUnlockStakeEpoch /= newUnlockStakeEpoch = do
          let bootstrap = isBootstrapEra oldUnlockStakeEpoch epoch
          unless bootstrap $ throwError
              PollBootstrapEraInvalidChange
              { pbeicLast = epoch
              , pbeicAdopted = oldUnlockStakeEpoch
              , pbeicProposed = newUnlockStakeEpoch
              , pbeicUpId = upId
              }
    | otherwise = pass

-- | Dummy type for tagging used by 'calcSoftforkThreshold'.
data CurEpoch

-- | Dummy type for tagging used by 'calcSoftforkThreshold'.
data ConfirmedEpoch

-- | Calculate how much stake issuers of blocks with some block
-- version should have to make this version adopted.
calcSoftforkThreshold ::
       SoftforkRule
    -> Coin
    -> Tagged CurEpoch EpochIndex
    -> Tagged ConfirmedEpoch EpochIndex
    -> Coin
calcSoftforkThreshold SoftforkRule {..} totalStake (untag -> curEpoch) (untag -> confirmedEpoch)
    | curEpoch < confirmedEpoch =
        error
            "calcSoftforkThreshold: logical error, curEpoch < confirmedEpoch, can't happen"
    | otherwise = applyCoinPortionUp portion totalStake
  where
    minuend :: Word64
    minuend = getCoinPortion srInitThd
    -- ↓ Can't overflow, because we explicitly check it above (↑). ↓
    epochDiff :: Word64
    epochDiff = fromIntegral $ curEpoch - confirmedEpoch
    -- ↓ Maximal epoch difference such that decrement can be
    -- calculated w/o overflow. ↓
    maxEpochDiff = coinPortionDenominator `div` getCoinPortion srThdDecrement
    -- ↓ Is evaluated only if 'epochDiff ≤ maxEpochDiff', in which
    -- case overflow is impossible, moreover, 'subtrahend' should
    -- represent a valid 'CoinPortion'. ↓
    subtrahend :: Word64
    subtrahend = getCoinPortion srThdDecrement * epochDiff
    portion
        -- ↓This condition↓ means that too many epochs passed so that
        -- decrement doesn't even fit into 'CoinPortion'.
        | epochDiff > maxEpochDiff = srMinThd
        -- 'subtrahend + getCoinPortion srMinThd' can not overflow as
        -- long as 2 'coinPortionDenominator's fit into 'Word64'
        -- (which is true).
        --
        -- ↓Here↓ 'CoinPortion' is safe because:
        -- • 'minued - subtrahend' can't underflow because it's ensured by
        --   the guard;
        -- • the value can't be negative, because the type is unsigned;
        -- • the value can't be greater than max possible one, because
        --   minuend represents a valid coin portion.
        | minuend > subtrahend + getCoinPortion srMinThd =
            CoinPortion (minuend - subtrahend)
        | otherwise = srMinThd

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
    :: (MonadError PollVerFailure m, WithLogger m)
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
    logDebug $ sformat (
        "New vote: upId = "%build%",\
        \voter = "%build%",\
        \vote = "%build%",\
        \voter stake = "%build)
        upId voter combinedMaybe stake
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
    logDebug $
        sformat ("Stakes of proposal "%build%" after vote: \
                 \positive "%build%",\
                 \negative: "%build)
                upId posStakeFinal negStakeFinal
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
    :: (MonadPoll m, IsMainHeader mainHeader)
    => Either SlotId mainHeader
    -> Coin
    -> [(UpdateVote, Coin)]
    -> UpdateProposal
    -> m ()
putNewProposal slotOrHeader totalStake votesAndStakes up = insertActiveProposal ps
  where
    slotId = either identity (view headerSlotL) slotOrHeader
    cd = either (const Nothing) (Just . view difficultyL) slotOrHeader
    totalPositive = sumCoins . map snd . filter (uvDecision . fst) $ votesAndStakes
    totalNegative = sumCoins . map snd . filter (not . uvDecision . fst) $ votesAndStakes
    blkHeaderHash = either (const Nothing) (Just . view headerHashG) slotOrHeader
    votes = HM.fromList . map convertVote $ votesAndStakes
    -- New proposal always has a fresh vote (not revote).
    convertVote (vote, _) = (uvKey vote, newVoteState (uvDecision vote))
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
