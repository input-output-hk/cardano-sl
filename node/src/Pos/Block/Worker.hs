{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Block processing related workers.

module Pos.Block.Worker
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Universum

import           Control.Lens                (ix)
import qualified Data.List.NonEmpty          as NE
import           Data.Time.Units             (Microsecond)
import           Formatting                  (Format, bprint, build, fixed, int, now,
                                              sformat, shown, (%))
import           Mockable                    (concurrently, delay)
import           Serokell.Util               (listJson, pairF, sec)
import           System.Wlog                 (logDebug, logInfo, logWarning)

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (calcChainQualityM, calcOverallChainQuality,
                                              createGenesisBlockAndApply,
                                              createMainBlockAndApply)
import           Pos.Block.Network.Announce  (announceBlock, announceBlockOuts)
import           Pos.Block.Network.Retrieval (retrievalWorker)
import           Pos.Block.Slog              (scCQOverallMonitorState, scCQkMonitorState,
                                              slogGetLastSlots)
import           Pos.Communication.Protocol  (OutSpecs, SendActions (..), Worker,
                                              WorkerSpec, onNewSlotWorker)
import           Pos.Constants               (criticalCQ, criticalCQBootstrap,
                                              networkDiameter, nonCriticalCQ,
                                              nonCriticalCQBootstrap)
import           Pos.Context                 (getOurPublicKey, recoveryCommGuard)
import           Pos.Core                    (HasCoreConstants, SlotId (..),
                                              Timestamp (Timestamp), blkSecurityParam,
                                              flattenSlotId, gbHeader, getSlotIndex,
                                              slotIdF, unflattenSlotId)
import           Pos.Core.Address            (addressHash)
import           Pos.Crypto                  (ProxySecretKey (pskDelegatePk, pskIssuerPk, pskOmega))
import           Pos.DB                      (gsIsBootstrapEra)
import           Pos.DB.Misc                 (getProxySecretKeysLight)
import           Pos.Delegation.Helpers      (isRevokePsk)
import           Pos.Delegation.Logic        (getDlgTransPsk)
import           Pos.Delegation.Types        (ProxySKBlockInfo)
import           Pos.GState                  (getPskByIssuer)
import           Pos.Lrc.DB                  (getLeaders)
import           Pos.Reporting               (DistrMonitor (..), DistrMonitorState,
                                              recordValue)
import           Pos.Slotting                (currentTimeSlotting,
                                              getSlotStartEmpatically)
import           Pos.Ssc.Class               (SscWorkersClass)
import           Pos.Util                    (logWarningSWaitLinear, mconcatPair)
import           Pos.Util.Chrono             (OldestFirst (..))
import           Pos.Util.JsonLog            (jlCreatedBlock)
import           Pos.Util.LogSafe            (logDebugS, logInfoS, logWarningS)
import           Pos.Util.TimeWarp           (CanJsonLog (..))
import           Pos.WorkMode.Class          (WorkMode)

----------------------------------------------------------------------------
-- All workers
----------------------------------------------------------------------------

-- | All workers specific to block processing.
blkWorkers
    :: (SscWorkersClass ssc, WorkMode ssc ctx m)
    => ([WorkerSpec m], OutSpecs)
blkWorkers =
    merge $ [ blkOnNewSlot
            , retrievalWorker
            ]
  where
    merge = mconcatPair . map (first pure)

-- Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode ssc ctx m => (WorkerSpec m, OutSpecs)
blkOnNewSlot =
    onNewSlotWorker True announceBlockOuts $ \slotId sendActions ->
        recoveryCommGuard $
        () <$
        chainQualityChecker slotId `concurrently`
        blockCreator slotId sendActions

----------------------------------------------------------------------------
-- Block creation worker
----------------------------------------------------------------------------

blockCreator
    :: WorkMode ssc ctx m
    => SlotId -> SendActions m -> m ()
blockCreator (slotId@SlotId {..}) sendActions = do

    -- First of all we create genesis block if necessary.
    mGenBlock <- createGenesisBlockAndApply siEpoch
    whenJust mGenBlock $ \createdBlk -> do
        logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
        jsonLog $ jlCreatedBlock (Left createdBlk)

    -- Then we get leaders for current epoch.
    -- Note: we are using non-blocking version here.  If we known
    -- genesis block for current epoch, then we either have calculated
    -- it before and it implies presense of leaders in MVar or we have
    -- read leaders from DB during initialization.
    leadersMaybe <- getLeaders siEpoch
    case leadersMaybe of
        -- If we don't know leaders, we can't do anything.
        Nothing -> logWarning "Leaders are not known for new slot"
        -- If we know leaders, we check whether we are leader and
        -- create a new block if we are. We also create block if we
        -- have suitable PSK.
        Just leaders ->
            maybe onNoLeader
                  (onKnownLeader leaders)
                  (leaders ^? ix (fromIntegral $ getSlotIndex siSlot))
  where
    onNoLeader =
        logWarning "Couldn't find a leader for current slot among known ones"
    logOnEpochFS = if siSlot == minBound then logInfoS else logDebugS
    logOnEpochF = if siSlot == minBound then logInfo else logDebug
    onKnownLeader leaders leader = do
        ourPk <- getOurPublicKey
        let ourPkHash = addressHash ourPk
        logOnEpochFS $ sformat ("Our pk: "%build%", our pkHash: "%build) ourPk ourPkHash
        logOnEpochF $ sformat ("Current slot leader: "%build) leader


        let -- position, how many to drop, list. This is to show some
            -- of leaders before and after current slot, but not all
            -- of them.
            dropAround :: Int -> Int -> [a] -> [a]
            dropAround p s = take (2*s + 1) . drop (max 0 (p - s))
            strLeaders = map (bprint pairF) (zip [0 :: Int ..] $ toList leaders)
        if siSlot == minBound
            then logInfo $ sformat ("Full slot leaders: "%listJson) strLeaders
            else logDebug $ sformat ("Trimmed leaders: "%listJson) $
                            dropAround (fromIntegral $ fromEnum $ siSlot) 10 strLeaders

        proxyCerts <- getProxySecretKeysLight
        let validCerts =
                filter (\pSk -> let (w0,w1) = pskOmega pSk
                                in and [ siEpoch >= w0
                                       , siEpoch <= w1
                                       , not $ isRevokePsk pSk
                                       ])
                       proxyCerts
            -- cert we can use to _issue_ instead of real slot leader
            validLightCert = find (\psk -> addressHash (pskIssuerPk psk) == leader &&
                                           pskDelegatePk psk == ourPk)
                             validCerts
            ourLightPsk = find (\psk -> pskIssuerPk psk == ourPk) validCerts
            lightWeDelegated = isJust ourLightPsk
        logDebugS $ sformat ("Available to use lightweight PSKs: "%listJson) validCerts

        ourHeavyPsk <- getPskByIssuer (Left ourPk)
        let heavyWeAreIssuer = isJust ourHeavyPsk
        dlgTransM <- getDlgTransPsk leader
        let finalHeavyPsk = snd <$> dlgTransM
        logDebug $ "End delegation psk for this slot: " <> maybe "none" pretty finalHeavyPsk
        let heavyWeAreDelegate = maybe False ((== ourPk) . pskDelegatePk) finalHeavyPsk

        if | heavyWeAreIssuer ->
                 logInfoS $ sformat
                 ("Not creating the block because it's delegated by heavy psk: "%build)
                 ourHeavyPsk
           | lightWeDelegated ->
                 logInfoS $ sformat
                 ("Not creating the block because it's delegated by light psk: "%build)
                 ourLightPsk
           | leader == ourPkHash ->
                 onNewSlotWhenLeader slotId Nothing sendActions
           | heavyWeAreDelegate ->
                 let pske = Right . swap <$> dlgTransM
                 in onNewSlotWhenLeader slotId pske sendActions
           | isJust validLightCert ->
                 onNewSlotWhenLeader slotId  (Left <$> validLightCert) sendActions
           | otherwise -> pass

onNewSlotWhenLeader
    :: WorkMode ssc ctx m
    => SlotId
    -> ProxySKBlockInfo
    -> Worker m
onNewSlotWhenLeader slotId pske SendActions {..} = do
    let logReason =
            sformat ("I have a right to create a block for the slot "%slotIdF%" ")
                    slotId
        logLeader = "because i'm a leader"
        logCert (Left psk) =
            sformat ("using ligtweight proxy signature key "%build%", will do it soon") psk
        logCert (Right (psk,_)) =
            sformat ("using heavyweight proxy signature key "%build%", will do it soon") psk
    logInfoS $ logReason <> maybe logLeader logCert pske
    nextSlotStart <- getSlotStartEmpatically (succ slotId)
    currentTime <- currentTimeSlotting
    let timeToCreate =
            max currentTime (nextSlotStart - Timestamp networkDiameter)
        Timestamp timeToWait = timeToCreate - currentTime
    logInfoS $
        sformat ("Waiting for "%shown%" before creating block") timeToWait
    delay timeToWait
    logWarningSWaitLinear 8 "onNewSlotWhenLeader" onNewSlotWhenLeaderDo
  where
    onNewSlotWhenLeaderDo = do
        logInfoS "It's time to create a block for current slot"
        createdBlock <- createMainBlockAndApply slotId pske
        either whenNotCreated whenCreated createdBlock
        logInfoS "onNewSlotWhenLeader: done"
    whenCreated createdBlk = do
            logInfoS $
                sformat ("Created a new block:\n" %build) createdBlk
            jsonLog $ jlCreatedBlock (Right createdBlk)
            void $ announceBlock enqueueMsg $ createdBlk ^. gbHeader
    whenNotCreated = logWarningS . (mappend "I couldn't create a new block: ")

----------------------------------------------------------------------------
-- Chain qualitiy checker
----------------------------------------------------------------------------

-- This action should be called only when we are synchronized with the
-- network.  The goal of this worker is not to detect violation of
-- chain quality assumption, but to produce warnings when this
-- assumption is close to being violated.
chainQualityChecker
    :: forall ssc ctx m. WorkMode ssc ctx m
    => SlotId -> m ()
chainQualityChecker curSlot = do
    OldestFirst lastSlots <- slogGetLastSlots
    -- If total number of blocks is less than `blkSecurityParam' we do
    -- nothing for two reasons:
    -- 1. Usually after we deploy cluster we monitor it manually for a while.
    -- 2. Sometimes we deploy after start time, so chain quality may indeed by
    --    poor right after launch.
    case nonEmpty lastSlots of
        Nothing -> pass
        Just slotsNE
            | length slotsNE < fromIntegral blkSecurityParam -> pass
            | otherwise -> chainQualityCheckerDo (NE.head slotsNE)
  where
    chainQualityCheckerDo kThSlot = do
        logDebug $ sformat ("Block with depth 'k' ("%int%
                            ") was created during slot "%slotIdF)
            blkSecurityParam (unflattenSlotId kThSlot)
        let curFlatSlot = flattenSlotId curSlot
        -- We use monadic version here, because it also does sanity
        -- check and we don't want to copy-paste it and it's easier
        -- and cheap.
        chainQualityK :: Double <- calcChainQualityM curFlatSlot
        isBootstrapEra <- gsIsBootstrapEra (siEpoch curSlot)
        monitorStateK <- view scCQkMonitorState
        monitorStateOverall <- view scCQOverallMonitorState
        let monitorK = cqkDistrMonitor monitorStateK isBootstrapEra
        let monitorOverall = cqOverallDistrMonitor monitorStateOverall
        recordValue monitorK chainQualityK
        whenJustM (calcOverallChainQuality @ssc) $ recordValue monitorOverall

-- Monitor for chain quality for last k blocks.
cqkDistrMonitor ::
       HasCoreConstants => DistrMonitorState -> Bool -> DistrMonitor
cqkDistrMonitor st isBootstrapEra =
    DistrMonitor
    { dmState = st
    , dmReportMisbehaviour = classifier
    , dmMisbehFormat =
          "Poor chain quality for the last 'k' ("%kFormat%") blocks, "%
          "less than "%now (bprint cqF criticalThreshold)%": "%cqF
    , dmDebugFormat =
          Just $ "Chain quality for the last 'k' ("%kFormat% ") blocks is "%cqF
    }
  where
    classifier :: Microsecond -> Maybe Double -> Double -> Maybe Bool
    classifier timePassed prevVal newVal
        -- report at most once per 400 sec, unless decreased
        | not decreased && timePassed < sec 400 = Nothing
        | newVal < criticalThreshold = Just True
        | newVal < nonCriticalThreshold = Just False
        | otherwise = Nothing
      where
        decreased = maybe False (newVal <) prevVal
    criticalThreshold
        | isBootstrapEra = criticalCQBootstrap
        | otherwise = criticalCQ
    nonCriticalThreshold
        | isBootstrapEra = nonCriticalCQBootstrap
        | otherwise = nonCriticalCQ
    -- Can be used to insert the value of 'blkSecurityParam' into a 'Format'.
    kFormat :: Format r r
    kFormat = now (bprint int blkSecurityParam)

cqOverallDistrMonitor :: DistrMonitorState -> DistrMonitor
cqOverallDistrMonitor st =
    DistrMonitor
    { dmState = st
    , dmReportMisbehaviour = classifier
    , dmMisbehFormat = cqF -- won't be used due to classifier
    , dmDebugFormat = Just $ "Overall chain quality is " %cqF
    }
  where
    classifier _ _ _ = Nothing

-- Private chain quality formatter.
cqF :: Format r (Double -> r)
cqF = fixed 3
