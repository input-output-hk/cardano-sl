-- | Version related checks of proposal:
-- * BlockVersionState
-- * BlockVersion
-- * Software version

module Pos.Update.Poll.Logic.Version
       ( verifyAndApplyProposalBVS
       , verifyBlockVersion
       , verifySoftwareVersion
       ) where

import           Control.Monad.Except       (MonadError, throwError)
import           Universum

import           Pos.Core                   (SoftwareVersion (..))
import           Pos.Update.Core            (BlockVersionData (..), UpId,
                                             UpdateProposal (..), upMaxBlockSize,
                                             upScriptVersion, upSlotDuration)
import           Pos.Update.Poll.Class      (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Failure    (PollVerFailure (..))
import           Pos.Update.Poll.Logic.Base (canBeProposedBV, verifyNextBVData)
import           Pos.Update.Poll.Types      (BlockVersionState (..))


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
verifyAndApplyProposalBVS
    :: (MonadError PollVerFailure m, MonadPoll m)
    => UpId -> UpdateProposal -> m ()
verifyAndApplyProposalBVS upId up =
    getBVD >>= \case
        -- This block version is already known, so we just check that
        -- everything is the same
        Just BlockVersionData {..}
            | bvdScriptVersion /= upScriptVersion up -> throwError
                  PollWrongScriptVersion
                      { pwsvExpected = bvdScriptVersion
                      , pwsvFound    = upScriptVersion up
                      , pwsvUpId     = upId }
            | bvdSlotDuration /= upSlotDuration up -> throwError
                  PollWrongSlotDuration
                      { pwsdExpected = bvdSlotDuration
                      , pwsdFound    = upSlotDuration up
                      , pwsdUpId     = upId }
            | bvdMaxBlockSize /= upMaxBlockSize up -> throwError
                  PollWrongMaxBlockSize
                      { pwmbsExpected = bvdMaxBlockSize
                      , pwmbsFound    = upMaxBlockSize up
                      , pwmbsUpId     = upId }
            | otherwise -> pass
        -- This block version isn't known, so we can add it after doing
        -- checks against the previous known block version state
        Nothing -> do
            let bvd = upBlockVersionData up
            let newBVS = BlockVersionState
                  { bvsData = bvd
                  , bvsIsConfirmed   = False
                  , bvsIssuersStable = mempty
                  , bvsIssuersUnstable = mempty
                  , bvsLastBlockStable = Nothing
                  , bvsLastBlockUnstable = Nothing
                  }
            oldBVD <- getAdoptedBVData
            verifyNextBVData upId oldBVD bvd
            putBVState (upBlockVersion up) newBVS
  where
    proposedBV = upBlockVersion up
    getBVD =
        maybe tryAdoptedBVD (pure . Just . bvsData) =<< getBVState proposedBV
    tryAdoptedBVD =
        getAdoptedBVFull <&> \case
            (bv, bvd)
                | bv == proposedBV -> Just bvd
                | otherwise -> Nothing

-- Here we verify that proposed protocol version could be proposed.
-- See documentation of 'Logic.Base.canBeProposedBV' for details.
verifyBlockVersion
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => UpId -> UpdateProposal -> m ()
verifyBlockVersion upId UnsafeUpdateProposal {..} = do
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
verifySoftwareVersion upId UnsafeUpdateProposal {..} =
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
