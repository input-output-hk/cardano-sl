{-# LANGUAGE TypeOperators #-}

-- | Softfork resolution logic.

module Pos.Update.Poll.Logic.Softfork
       ( recordBlockIssuance
       , processGenesisBlock
       ) where

import           Universum

import           Control.Monad.Except (MonadError, throwError)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import           Data.Tagged (Tagged (..))
import           Formatting (build, sformat, (%))
import           Serokell.Util.Text (listJson)
import           System.Wlog (logInfo)

import           Pos.Core (BlockVersion, Coin, EpochIndex, HeaderHash,
                           SlotId (..), SoftforkRule (..), StakeholderId, crucialSlot, sumCoins,
                           unsafeIntegerToCoin, HasProtocolConstants)
import           Pos.Core.Update (BlockVersionData (..))
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Failure (PollVerFailure (..))
import           Pos.Update.Poll.Logic.Base (ConfirmedEpoch, CurEpoch, adoptBlockVersion,
                                             calcSoftforkThreshold, canBeAdoptedBV,
                                             updateSlottingData)
import           Pos.Update.Poll.Types (BlockVersionState (..))
import           Pos.Util.AssertMode (inAssertMode)

-- | Record the fact that main block with given version and leader has
-- been issued by for the given slot.
recordBlockIssuance
    :: (MonadError PollVerFailure m, MonadPoll m, HasProtocolConstants)
    => StakeholderId -> BlockVersion -> SlotId -> HeaderHash -> m ()
recordBlockIssuance id bv slot h = do
    -- Issuance is stable if it happens before crucial slot for next epoch.
    -- In other words, processing genesis block for next epoch will
    -- inevitably encounter this issuer.
    let unstable = slot > crucialSlot (siEpoch slot + 1)
    getBVState bv >>= \case
        Nothing -> unlessM ((bv ==) <$> getAdoptedBV) $ throwError noBVError
        Just bvs@BlockVersionState {..}
            | id `HS.member` bvsIssuersStable -> pass
            | id `HS.member` bvsIssuersUnstable && unstable -> pass
            | id `HS.member` bvsIssuersUnstable -> throwError unstableNotEmpty
            | otherwise -> putBVState bv $ newBVS bvs unstable
  where
    noBVError =
        PollInternalError $
        sformat
            ("someone issued a block with unconfirmed and not adopted block version ("
             %build%") and we are recording this fact now") bv
    unstableNotEmpty =
        PollInternalError $
        sformat
            ("bvsIssuersUnstable is not empty while we are processing slot"%
             " before a crucial one (block is "%build%")") h
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
    :: forall m. (MonadError PollVerFailure m, MonadPoll m, HasProtocolConstants)
    => EpochIndex -> m ()
processGenesisBlock epoch = do
    -- First thing to do is to obtain values threshold for softfork
    -- resolution rule check.
    totalStake <- note (PollUnknownStakes epoch) =<< getEpochTotalStake epoch
    BlockVersionData {..} <- getAdoptedBVData
    -- Then we take all competing BlockVersions and actually check softfork
    -- resolution rule for them.
    competing <- getCompetingBVStates
    logCompetingBVStates competing
    let checkThreshold' = checkThreshold totalStake bvdSoftforkRule
    toAdoptList <- catMaybes <$> mapM checkThreshold' competing
    logWhichCanBeAdopted $ map fst toAdoptList
    -- We also do sanity check in assert mode just in case.
    inAssertMode $ sanityCheckCompeting $ map fst competing
    case nonEmpty toAdoptList of
        -- If there is nothing to adopt, we move unstable issuers to stable
        -- and that's all.
        Nothing                         -> mapM_ moveUnstable competing
        -- Otherwise we choose version to adopt, adopt it, remove all
        -- versions which no longer can be adopted and only then move
        -- unstable to stable.
        Just (chooseToAdopt -> toAdopt) -> adoptAndFinish competing toAdopt
    -- In the end we also update slotting data to the most recent state.
    updateSlottingData epoch
    setEpochProposers mempty
  where
    checkThreshold ::
           Coin
        -> SoftforkRule
        -> (BlockVersion, BlockVersionState)
        -> m $ Maybe (BlockVersion, BlockVersionState)
    checkThreshold totalStake sr bvData@(bv, bvs) = do
        let onNoConfirmedEpoch =
                PollInternalError $
                sformat
                    ("checkThresholdDo: block version " %build %
                     " is not competing, hello!")
                    bv
        confirmedEpoch <- note onNoConfirmedEpoch (bvsConfirmedEpoch bvs)
        checkThresholdDo totalStake sr confirmedEpoch bvData <$>
            calculateIssuersStake epoch bvs
    checkThresholdDo ::
           Coin
        -> SoftforkRule
        -> EpochIndex
        -> (BlockVersion, BlockVersionState)
        -> Coin
        -> Maybe (BlockVersion, BlockVersionState)
    checkThresholdDo totalStake sr confirmedEpoch bvData issuersStake =
        let thd =
                calcSoftforkThreshold
                    sr
                    totalStake
                    (Tagged @CurEpoch epoch)
                    (Tagged @ConfirmedEpoch confirmedEpoch)
        in bvData <$ guard (issuersStake >= thd)
    adoptAndFinish allConfirmed (bv, BlockVersionState {..}) = do
        winningBlock <-
            note (PollInternalError "no winning block") bvsLastBlockStable
        adoptBlockVersion winningBlock bv
        filterBVAfterAdopt (fst <$> allConfirmed)
        mapM_ moveUnstable =<< getCompetingBVStates
    logCompetingBVStates [] =
        logInfo ("We are processing genesis block, currently we don't have " <>
                "competing block versions")
    logCompetingBVStates versions = do
        logInfo $ sformat
                  ("We are processing genesis block, "%
                   "competing block versions are: "%listJson)
                  (map fst versions)
        mapM_ logBVIssuers versions
    logBVIssuers (bv, BlockVersionState {..}) =
        logInfo $ sformat (build%" has these stable issuers "%listJson%
                           " and these unstable issuers "%listJson)
                           bv bvsIssuersStable bvsIssuersUnstable
    logWhichCanBeAdopted =
        logInfo . sformat ("These versions can be adopted: "%listJson)

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
chooseToAdopt = NE.last . NE.sortWith fst

-- This function removes 'BlockVersion's which can't be adopted anymore.
filterBVAfterAdopt :: MonadPoll m => [(BlockVersion)] -> m ()
filterBVAfterAdopt = mapM_ filterBVAfterAdoptDo
  where
    filterBVAfterAdoptDo bv = unlessM (canBeAdoptedBV bv) $ delBVState bv

-- Here we check that all competing versions satisfy 'canBeAdoptedBV' predicate.
sanityCheckCompeting
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => [BlockVersion] -> m ()
sanityCheckCompeting = mapM_ sanityCheckConfirmedDo
  where
    sanityCheckConfirmedDo bv = unlessM (canBeAdoptedBV bv) $
        throwError $ PollInternalError $ sformat fmt bv
    fmt = "we have competing block version which doesn't satisfy "%
          "'canBeAdoptedBV' predicate: "%build%" :unamused:"
