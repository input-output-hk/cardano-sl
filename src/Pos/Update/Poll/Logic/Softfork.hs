-- | Softfork resolution logic.

module Pos.Update.Poll.Logic.Softfork
       ( recordBlockIssuance
       , processGenesisBlock
       ) where

import           Control.Monad.Except       (MonadError, throwError)
import qualified Data.HashSet               as HS
import           Data.List.NonEmpty         (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Formatting                 (build, sformat, (%))
import           Universum

import           Pos.Constants              (usSoftforkThreshold)
import           Pos.Types                  (BlockVersion, Coin, EpochIndex, HeaderHash,
                                             SlotId (..), StakeholderId, applyCoinPortion,
                                             crucialSlot, sumCoins, unsafeIntegerToCoin)
import           Pos.Update.Poll.Class      (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Logic.Base (adoptBlockVersion, canBeProposedBV)
import           Pos.Update.Poll.Types      (BlockVersionState (..), PollVerFailure (..))

-- | Record the fact that main block with given version has been issued by
-- stakeholder with given id for the given slot.
recordBlockIssuance
    :: (MonadError PollVerFailure m, MonadPoll m)
    => StakeholderId -> BlockVersion -> SlotId -> HeaderHash -> m ()
recordBlockIssuance id bv slot h = do
    let unstable = slot > crucialSlot (siEpoch slot)
    bvs@BlockVersionState {..} <- note noBVError =<< getBVState bv
    if | id `HS.member` bvsIssuersStable -> pass
       | id `HS.member` bvsIssuersUnstable && unstable -> pass
       | id `HS.member` bvsIssuersUnstable -> throwError unstableNotEmpty
       | otherwise -> putBVState bv $ newBVS bvs unstable
  where
    noBVError =
        PollInternalError $
        sformat
            ("someone issued a block with unconfirmed block version (" %build %
             ") and we are recording this fact now")
            bv
    unstableNotEmpty =
        PollInternalError $
        sformat
            ("bvsIssuersUnstable is not empty while we are processing slot"%
             " before a crucial one (block is "%
             build%")") h
    newBVS bvs@BlockVersionState {..} unstable
        | unstable =
            bvs
            { bvsIssuersUnstable = HS.insert id bvsIssuersUnstable
            , bvsLastBlockUnstable = Just h
            }
        | otherwise =
            bvs
            { bvsIssuersStable = HS.insert id bvsIssuersStable
            , bvsLastBlockStable = Just h
            }

-- | Process creation of genesis block for given epoch.
processGenesisBlock
    :: (MonadError PollVerFailure m, MonadPoll m)
    => EpochIndex -> m ()
processGenesisBlock epoch = do
    -- First thing to do is to find out threshold for softfork resolution rule.
    totalStake <- note (PollUnknownStakes epoch) =<< getEpochTotalStake epoch
    let threshold = applyCoinPortion usSoftforkThreshold totalStake
    -- Then we take all confirmed BlockVersions and check softfork
    -- resolution rule for them.
    confirmed <- getConfirmedBVStates
    toAdoptList <- catMaybes <$> mapM (checkThreshold threshold) confirmed
    case nonEmpty toAdoptList of
        -- If there is nothing to adopt, we move unstable issuers to stable
        -- and that's all.
        Nothing                         -> mapM_ moveUnstable confirmed
        -- Otherwise we choose version to adopt, adopt it, remove all
        -- versions which no longer can be adopted and only then move
        -- unstable to stable.
        Just (chooseToAdopt -> toAdopt) -> adoptAndFinish confirmed toAdopt
  where
    checkThreshold thd (bv, bvs) =
        chechThresholdDo thd (bv, bvs) <$> calculateIssuersStake epoch bvs
    chechThresholdDo thd (bv, bvs) stake
        | stake >= thd = Just (bv, bvs)
        | otherwise = Nothing
    adoptAndFinish allConfirmed (bv, BlockVersionState {..}) = do
        winningBlock <-
            note (PollInternalError "no winning block") bvsLastBlockStable
        adoptBlockVersion winningBlock bv
        filterBVAfterAdopt (fst <$> allConfirmed)
        mapM_ moveUnstable =<< getConfirmedBVStates

calculateIssuersStake
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => EpochIndex -> BlockVersionState -> m Coin
calculateIssuersStake epoch BlockVersionState {..} =
    unsafeIntegerToCoin . sumCoins <$>
    mapM resolveStake (toList bvsIssuersStable)
  where
    resolveStake id =
        note (PollInternalError $ unknownStakeMsg id) =<<
        getBlockIssuerStake epoch id
    unknownStakeMsg =
        sformat
            ("stake for epoch " %build % " is unknown for stable issuer " %build)
            epoch

-- This function moves unstable issuers and unstable last block to stable.
moveUnstable
    :: MonadPoll m
    => (BlockVersion, BlockVersionState) -> m ()
moveUnstable (bv, bvs@BlockVersionState {..}) =
    putBVState
        bv
        bvs
        { bvsIssuersStable = bvsIssuersStable <> bvsIssuersUnstable
        , bvsLastBlockStable = bvsLastBlockUnstable <|> bvsLastBlockStable
        , bvsIssuersUnstable = mempty
        , bvsLastBlockUnstable = Nothing
        }

-- This function chooses 'BlockVersion' to adopt when there are
-- multiple options.
chooseToAdopt :: NonEmpty (BlockVersion, BlockVersionState)
              -> (BlockVersion, BlockVersionState)
chooseToAdopt = NE.head

-- This function removes 'BlockVersion's which can't be adopted anymore.
filterBVAfterAdopt :: MonadPoll m => [(BlockVersion)] -> m ()
filterBVAfterAdopt = mapM_ filterBVAfterAdoptDo
  where
    filterBVAfterAdoptDo bv = unlessM (canBeProposedBV bv) $ delBVState bv
