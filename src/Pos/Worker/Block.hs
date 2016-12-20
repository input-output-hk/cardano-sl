{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Block processing related workers.

module Pos.Worker.Block
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens              (ix, view, (^.), (^?))
import           Control.TimeWarp.Timed    (Microsecond, for, repeatForever, wait)
import qualified Data.HashMap.Strict       as HM
import           Data.Tagged               (untag)
import           Formatting                (build, sformat, shown, (%))
import           Serokell.Util             (VerificationRes (..), listJson)
import           Serokell.Util.Exceptions  ()
import           System.Wlog               (dispatchEvents, logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Communication.Methods (announceBlock)
import           Pos.Constants             (networkDiameter, slotDuration)
import           Pos.Context               (getNodeContext, ncPropagation,
                                            ncProxySecretKeys, ncProxyStorage,
                                            ncPublicKey, ncSecretKey)
import           Pos.Crypto                (ProxySecretKey, pskIssuerPk, pskOmega)
import           Pos.Slotting              (MonadSlots (getCurrentTime), getSlotStart)
import           Pos.Ssc.Class             (sscApplyGlobalState, sscGetLocalPayload,
                                            sscVerifyPayload)
import           Pos.State                 (createNewBlock, getGlobalMpcData,
                                            getHeadBlock, getLeaders, processNewSlot)
import           Pos.Txp.LocalData         (getLocalTxs)
import           Pos.Types                 (EpochIndex, SlotId (..),
                                            Timestamp (Timestamp), blockMpc, gbHeader,
                                            makePubKeyAddress, slotIdF)
import           Pos.Util                  (logWarningWaitLinear)
import           Pos.Util.JsonLog          (jlCreatedBlock, jlLog)
import           Pos.WorkMode              (WorkMode)
import Pos.Block.Logic (withBlkSemaphore)

-- | Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode ssc m => SlotId -> m ()
blkOnNewSlot slotId@SlotId {..} = do
    -- First of all we create genesis block if necessary.
    (mGenBlock, pnsLog) <- processNewSlot slotId
    dispatchEvents pnsLog
    forM_ mGenBlock $ \createdBlk -> do
        logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
        jlLog $ jlCreatedBlock (Left createdBlk)

    -- Then we get leaders for current epoch.
    leadersMaybe <- getLeaders siEpoch
    case leadersMaybe of
        -- If we don't know leaders, we can't do anything.
        Nothing -> logWarning "Leaders are not known for new slot"
        -- If we know leaders, we check whether we are leader and
        -- create a new block if we are.
        Just leaders -> do
            let logLeadersF = if siSlot == 0 then logInfo else logDebug
            logLeadersF (sformat ("Slot leaders: " %listJson) leaders)
            ourPkAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
            let leader = leaders ^? ix (fromIntegral siSlot)
            proxyCerts <-
                (\v -> view ncProxySecretKeys <$> liftIO (readMVar v)) =<<
                ncProxyStorage <$> getNodeContext
            let validCerts =
                    filter (\pSk -> let (w0,w1) = pskOmega pSk
                                    in siEpoch >= w0 && siEpoch <= w1) proxyCerts
                validCert =
                    find (\pSk -> Just (makePubKeyAddress $ pskIssuerPk pSk) == leader)
                         validCerts
            if | leader == Just ourPkAddr -> onNewSlotWhenLeader slotId Nothing
               | isJust validCert -> onNewSlotWhenLeader slotId validCert
               | otherwise -> pure ()

onNewSlotWhenLeader
    :: WorkMode ssc m
    => SlotId
    -> Maybe (ProxySecretKey (EpochIndex, EpochIndex))
    -> m ()
onNewSlotWhenLeader slotId pSk = do
    logInfo $
        maybe
        (sformat
            ("I'm the leader for the slot " %slotIdF % ", will create block soon.")
            slotId)
        (sformat
            ("I have a right to create a delegated block for the slot "%slotIdF%
             "using proxy signature key"%build%", will do it soon") slotId)
        pSk
    whenJust pSk $ logInfo . sformat ("Will use proxy signature key "%build)
    nextSlotStart <- getSlotStart (succ slotId)
    currentTime <- getCurrentTime
    let timeToCreate =
            max currentTime (nextSlotStart - Timestamp networkDiameter)
        Timestamp timeToWait = timeToCreate - currentTime
    logInfo $
        sformat ("Waiting for "%shown%" before creating block") timeToWait
    wait (for timeToWait)
    let verifyCreatedBlock blk =
            untag sscVerifyPayload
            (blk ^. gbHeader) (blk ^. blockMpc)
    let onNewSlotWhenLeaderDo = do
            logInfo "It's time to create a block for current slot"
            sk <- ncSecretKey <$> getNodeContext
            let whenCreated createdBlk = do
                    logInfo $
                        sformat ("Created a new block:\n" %build) createdBlk
                    jlLog $ jlCreatedBlock (Right createdBlk)
                    case verifyCreatedBlock createdBlk of
                        VerSuccess -> return ()
                        VerFailure warnings -> logWarning $ sformat
                            ("New block failed some checks: "%listJson)
                            warnings
                    globalData <- logWarningWaitLinear 6 "getGlobalMpcData"
                        getGlobalMpcData
                    logWarningWaitLinear 7 "sscApplyGlobalState" $
                        sscApplyGlobalState globalData
                    announceBlock $ createdBlk ^. gbHeader
            let whenNotCreated = logWarning . (mappend "I couldn't create a new block: ")
            sscData <- sscGetLocalPayload slotId
            localTxs <- HM.toList <$> getLocalTxs
            either whenNotCreated whenCreated =<< createNewBlock localTxs sk pSk slotId sscData
    logWarningWaitLinear 8 "onNewSlotWhenLeader" onNewSlotWhenLeaderDo

-- | All workers specific to block processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
blkWorkers :: WorkMode ssc m => [m ()]
blkWorkers = [blocksTransmitter]

blocksTransmitterInterval :: Microsecond
blocksTransmitterInterval = slotDuration `div` 2

blocksTransmitter :: WorkMode ssc m => m ()
blocksTransmitter = whenM (ncPropagation <$> getNodeContext) impl
  where
    impl = repeatForever blocksTransmitterInterval onError $
        do headBlock <- getHeadBlock
           case headBlock of
               Left _          -> logDebug "Head block is genesis block ⇒ no announcement"
               Right mainBlock -> announceBlock (mainBlock ^. gbHeader)
    onError e =
        blocksTransmitterInterval <$
        logWarning (sformat ("Error occured in blocksTransmitter: " %build) e)

lpcOnNewSlot :: WorkMode ssc m => SlotId -> m () --Leaders and Participants computation
lpcOnNewSlot slotId@SlotId{..} = withBlkSemaphore $ \tip -> do notImplemented

