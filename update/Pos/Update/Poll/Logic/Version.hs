{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}

-- | Version related checks of proposal:
-- * BlockVersionState
-- * BlockVersion
-- * Software version

module Pos.Update.Poll.Logic.Version
       ( verifyAndApplyProposalBVS
       , verifyBlockVersion
       , verifySoftwareVersion
       ) where

import           Control.Monad.Except (MonadError, throwError)
import           Universum

import           Pos.Core (EpochIndex, SoftwareVersion (..))
import           Pos.Core.Update (BlockVersionData (..), BlockVersionModifier (..), UpId,
                                  UpdateProposal (..))
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Failure (PollVerFailure (..))
import           Pos.Update.Poll.Logic.Base (canBeProposedBV, verifyNextBVMod)
import           Pos.Update.Poll.Types (BlockVersionState (..))


-- Here we add check that block version data from proposal is consistent
-- with current data and add new 'BlockVersionState' if this is a new
-- version.
--
-- The following checks are performed:
--
-- 1. We check that versions and constants from proposal are the same as
-- versions and constants from other proposals with the same block version.
--
-- 2. But if the proposal has a new 'BlockVersion', we check that
-- a) its 'ScriptVersion' is @lastScriptVersion + 1@, and
-- b) its 'maxBlockSize' is at most 2× the previous block size.
-- c) its 'unlockStakeEpoch' does not affect past transactions. That is, we
--    cannot change the end of the boostrap era post factum.
verifyAndApplyProposalBVS
    :: forall m. (MonadError PollVerFailure m, MonadPoll m)
    => UpId
    -> EpochIndex -- block epoch index
    -> UpdateProposal
    -> m ()
verifyAndApplyProposalBVS upId epoch up =
    getBVDataOrModifier >>= \case
        -- This block version is adopted, so we check
        -- 'BlockVersionModifier' against the adopted 'BlockVersionData'.
        Just (Left adoptedBVD) -> unless (bvmMatchesBVD proposedBVM adoptedBVD) $
           throwError PollAlreadyAdoptedDiffers
                      { paadProposed = proposedBVM
                      , paadAdopted = adoptedBVD
                      , paadUpId = upId
                      }
        -- This block version is competing, so we check that
        -- 'BlockVersionModifier' is the same.
        Just (Right competingBVM) -> unless (competingBVM == proposedBVM) $
            throwError PollInconsistentBVM
                       { pibExpected = competingBVM
                       , pibFound = proposedBVM
                       , pibUpId = upId
                       }
        -- This block version isn't known, so we can add it after doing
        -- checks against the previous known block version state
        Nothing -> do
            let newBVS = BlockVersionState
                  { bvsModifier          = proposedBVM
                  , bvsConfirmedEpoch    = Nothing
                  , bvsIssuersStable     = mempty
                  , bvsIssuersUnstable   = mempty
                  , bvsLastBlockStable   = Nothing
                  , bvsLastBlockUnstable = Nothing
                  }
            oldBVD <- getAdoptedBVData
            verifyNextBVMod upId epoch oldBVD proposedBVM
            putBVState (upBlockVersion up) newBVS
  where
    proposedBV = upBlockVersion up
    proposedBVM = upBlockVersionMod up
    -- We have three different cases:
    --
    -- • proposed 'BlockVersion' might be completely new, in this case
    -- we check that proposed modifier can follow the adopted block
    -- version data;
    --
    -- • proposed 'BlockVersion' might be the adopted one,
    -- in this case we check that proposed modifier corresponds to the
    -- adopted block version data;
    --
    -- • proposed 'BlockVersion' might be not adopted but competing,
    -- in this case we check that proposed modifier is the same as the
    -- one associated with the proposed block version.
    getBVDataOrModifier ::
           m $ Maybe $ Either BlockVersionData BlockVersionModifier
    getBVDataOrModifier =
        -- maybe tryAdoptedBVD (pure . pure . pure . bvsModifier) =<<
        maybe tryAdoptedBVD (pure . Just . Right . bvsModifier) =<<
        getBVState proposedBV
    tryAdoptedBVD =
        getAdoptedBVFull <&> \case
            (bv, bvd)
                | bv == proposedBV -> Just (Left bvd)
                | otherwise -> Nothing

-- Check whether all filled fields from 'BlockVersionModifier'
-- correspond to their analogues from 'BlockVersionData'.
bvmMatchesBVD :: BlockVersionModifier -> BlockVersionData -> Bool
-- Note: record wild cards and all other approaches to pattern
-- matching are not used here, because we want to have a warning if we
-- add something to these types and forget to update this function.
bvmMatchesBVD
          ( BlockVersionModifier
    bvmScriptVersion
    bvmSlotDuration
    bvmMaxBlockSize
    bvmMaxHeaderSize
    bvmMaxTxSize
    bvmMaxProposalSize
    bvmMpcThd
    bvmHeavyDelThd
    bvmUpdateVoteThd
    bvmUpdateProposalThd
    bvmUpdateImplicit
    bvmSoftforkRule
    bvmTxFeePolicy
    bvmUnlockStakeEpoch
        ) ( BlockVersionData
    bvdScriptVersion
    bvdSlotDuration
    bvdMaxBlockSize
    bvdMaxHeaderSize
    bvdMaxTxSize
    bvdMaxProposalSize
    bvdMpcThd
    bvdHeavyDelThd
    bvdUpdateVoteThd
    bvdUpdateProposalThd
    bvdUpdateImplicit
    bvdSoftforkRule
    bvdTxFeePolicy
    bvdUnlockStakeEpoch
          ) =
          and [
      maybe True (== bvdScriptVersion)     bvmScriptVersion
    , maybe True (== bvdSlotDuration)      bvmSlotDuration
    , maybe True (== bvdMaxBlockSize)      bvmMaxBlockSize
    , maybe True (== bvdMaxHeaderSize)     bvmMaxHeaderSize
    , maybe True (== bvdMaxTxSize)         bvmMaxTxSize
    , maybe True (== bvdMaxProposalSize)   bvmMaxProposalSize
    , maybe True (== bvdMpcThd)            bvmMpcThd
    , maybe True (== bvdHeavyDelThd)       bvmHeavyDelThd
    , maybe True (== bvdUpdateVoteThd)     bvmUpdateVoteThd
    , maybe True (== bvdUpdateProposalThd) bvmUpdateProposalThd
    , maybe True (== bvdUpdateImplicit)    bvmUpdateImplicit
    , maybe True (== bvdSoftforkRule)      bvmSoftforkRule
    , maybe True (== bvdTxFeePolicy)       bvmTxFeePolicy
    , maybe True (== bvdUnlockStakeEpoch)  bvmUnlockStakeEpoch
    ]

-- Here we verify that proposed protocol version could be proposed.
-- See documentation of 'Logic.Base.canBeProposedBV' for details.
verifyBlockVersion
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => UpId -> UpdateProposal -> m ()
verifyBlockVersion upId UncheckedUpdateProposal {..} = do
    lastAdopted <- getAdoptedBV
    unlessM (canBeProposedBV upBlockVersion) $
        throwError
            PollBadBlockVersion
            { pbpvUpId = upId
            , pbpvGiven = upBlockVersion
            , pbpvAdopted = lastAdopted
            }

-- Here we check that software version is 1 more than last confirmed
-- version of given application. Or 0 if it's new application.
verifySoftwareVersion
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => UpId -> UpdateProposal -> m ()
verifySoftwareVersion upId UncheckedUpdateProposal {..} =
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
