{-# LANGUAGE TypeFamilies #-}

-- | Block processing related workers.

module Pos.Worker.Block
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens              (ix, (^.), (^?))
import           Control.TimeWarp.Logging  (logDebug, logInfo, logWarning)
import           Control.TimeWarp.Timed    (Microsecond, for, repeatForever, wait)
import           Formatting                (build, sformat, (%))
import           Serokell.Util             (VerificationRes (..), listJson, verifyGeneric)
import           Serokell.Util.Exceptions  ()
import           Universum

import           Pos.Communication.Methods (announceBlock)
import           Pos.Constants             (networkDiameter, slotDuration)
import           Pos.Slotting              (MonadSlots (getCurrentTime), getSlotStart)
import           Pos.Ssc.DynamicState      (isCommitmentId, isOpeningId, isSharesId,
                                            mdCommitments, mdOpenings, mdShares)
import           Pos.State                 (createNewBlock, getHeadBlock, getLeaders)
import           Pos.Types                 (SlotId (..), Timestamp (Timestamp), blockMpc,
                                            gbHeader, slotIdF)
import           Pos.Util                  (logWarningWaitLinear)
import           Pos.WorkMode              (WorkMode, getNodeContext, ncPublicKey,
                                            ncSecretKey)

-- | Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode m => SlotId -> m ()
blkOnNewSlot slotId@SlotId {..} = do
    leadersMaybe <- getLeaders siEpoch
    case leadersMaybe of
        Nothing -> logWarning "Leaders are not known for new slot"
        Just leaders -> do
            let logLeadersF = if siSlot == 0 then logInfo else logDebug
            logLeadersF (sformat ("Slot leaders: " %listJson) leaders)
            ourPk <- ncPublicKey <$> getNodeContext
            let leader = leaders ^? ix (fromIntegral siSlot)
            when (leader == Just ourPk) $ onNewSlotWhenLeader slotId

onNewSlotWhenLeader :: WorkMode m => SlotId -> m ()
onNewSlotWhenLeader slotId = do
    logInfo $
        sformat
            ("I am leader of " %slotIdF % ", I will create block soon")
            slotId
    nextSlotStart <- getSlotStart (succ slotId)
    currentTime <- getCurrentTime
    let timeToCreate =
            max currentTime (nextSlotStart - Timestamp networkDiameter)
        Timestamp timeToWait = timeToCreate - currentTime
    wait (for timeToWait)
    -- TODO: perhaps we could reuse mpcVerifyBlock for 'verifyCreatedBlock',
    -- or at least refactor the common parts out of it.
    let verifyCreatedBlock blk = verifyGeneric $
            let implies a b = not a || b
                isComm  = isCommitmentId slotId
                isOpen  = isOpeningId slotId
                isShare = isSharesId slotId
                hasNoComm  = null $ blk ^. blockMpc . mdCommitments
                hasNoOpen  = null $ blk ^. blockMpc . mdOpenings
                hasNoShare = null $ blk ^. blockMpc . mdShares
            in [ (isComm `implies` hasNoOpen,
                      "commitments block has openings")
               , (isComm `implies` hasNoShare,
                      "commitments block has shares")
               , (isOpen `implies` hasNoComm,
                      "openings block has commitments")
               , (isOpen `implies` hasNoShare,
                      "openings block has shares")
               , (isShare `implies` hasNoComm,
                      "shares block has commitments")
               , (isShare `implies` hasNoOpen,
                      "shares block has openings")
               ]
    let onNewSlotWhenLeaderDo = do
            logInfo "It's time to create a block for current slot"
            sk <- ncSecretKey <$> getNodeContext
            let whenCreated createdBlk = do
                    logInfo $
                        sformat ("Created a new block:\n" %build) createdBlk
                    case verifyCreatedBlock createdBlk of
                        VerSuccess -> return ()
                        VerFailure warnings -> logWarning $ sformat
                            ("New block failed some checks: "%listJson)
                            warnings
                    announceBlock $ createdBlk ^. gbHeader
            let whenNotCreated = logWarning "I couldn't create a new block"
            maybe whenNotCreated whenCreated =<< createNewBlock sk slotId
    logWarningWaitLinear 8 "onNewSlotWhenLeader" onNewSlotWhenLeaderDo

-- | All workers specific to block processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
blkWorkers :: WorkMode m => [m ()]
blkWorkers = [blocksTransmitter]

blocksTransmitterInterval :: Microsecond
blocksTransmitterInterval = slotDuration `div` 2

blocksTransmitter :: WorkMode m => m ()
blocksTransmitter =
    repeatForever blocksTransmitterInterval onError $
    do headBlock <- getHeadBlock
       case headBlock of
           Left _          -> logDebug "Head block is genesis block ⇒ no announcement"
           Right mainBlock -> announceBlock (mainBlock ^. gbHeader)
  where
    onError e =
        blocksTransmitterInterval <$
        logWarning (sformat ("Error occured in blocksTransmitter: " %build) e)
