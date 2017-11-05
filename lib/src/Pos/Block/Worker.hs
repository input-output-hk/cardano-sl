{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Block processing related workers.

module Pos.Block.Worker
       ( blkWorkers
       ) where

import           Universum

import           Control.Lens                (ix)
import qualified Data.List.NonEmpty          as NE
import           Data.Time.Units             (Microsecond)
import           Formatting                  (Format, bprint, build, fixed, int, now,
                                              sformat, shown, (%))
import           Mockable                    (delay, fork)
import           Serokell.Util               (listJson, pairF, sec)
import qualified System.Metrics.Label        as Label
import           System.Wlog                 (logDebug, logInfo, logWarning)

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (calcChainQualityFixedTime,
                                              calcChainQualityM, calcOverallChainQuality,
                                              createGenesisBlockAndApply,
                                              createMainBlockAndApply)
import           Pos.Block.Network.Announce  (announceBlock, announceBlockOuts)
import           Pos.Block.Network.Retrieval (retrievalWorker)
import           Pos.Block.Slog              (scCQFixedMonitorState,
                                              scCQOverallMonitorState, scCQkMonitorState,
                                              scCrucialValuesLabel,
                                              scDifficultyMonitorState,
                                              scEpochMonitorState,
                                              scGlobalSlotMonitorState,
                                              scLocalSlotMonitorState, slogGetLastSlots)
import           Pos.Communication.Protocol  (OutSpecs, SendActions (..), Worker,
                                              WorkerSpec, onNewSlotWorker)
import           Pos.Configuration           (networkDiameter)
import           Pos.Context                 (getOurPublicKey, recoveryCommGuard)
import           Pos.Core                    (BlockVersionData (..), ChainDifficulty,
                                              FlatSlotId, SlotId (..),
                                              Timestamp (Timestamp), blkSecurityParam,
                                              difficultyL, epochSlots, fixedTimeCQSec,
                                              flattenSlotId, gbHeader, getSlotIndex,
                                              slotIdF, unflattenSlotId)
import           Pos.Core.Address            (addressHash)
import           Pos.Core.Configuration      (HasConfiguration, criticalCQ,
                                              criticalCQBootstrap, nonCriticalCQ,
                                              nonCriticalCQBootstrap)
import           Pos.Crypto                  (ProxySecretKey (pskDelegatePk, pskIssuerPk, pskOmega))
import           Pos.DB                      (gsIsBootstrapEra)
import qualified Pos.DB.DB                   as DB
import           Pos.DB.Misc                 (getProxySecretKeysLight)
import           Pos.Delegation.Helpers      (isRevokePsk)
import           Pos.Delegation.Logic        (getDlgTransPsk)
import           Pos.Delegation.Types        (ProxySKBlockInfo)
import           Pos.GState                  (getAdoptedBVData, getPskByIssuer)
import           Pos.Lrc.DB                  (getLeaders)
import           Pos.Reporting               (MetricMonitor (..), MetricMonitorState,
                                              noReportMonitor, recordValue, reportOrLogE)
import           Pos.Slotting                (currentTimeSlotting,
                                              getSlotStartEmpatically)
import           Pos.Util                    (logWarningSWaitLinear, mconcatPair)
import           Pos.Util.Chrono             (OldestFirst (..))
import           Pos.Util.JsonLog            (jlCreatedBlock)
import           Pos.Util.LogSafe            (logDebugS, logInfoS, logWarningS)
import           Pos.Util.TimeWarp           (CanJsonLog (..))
import           Pos.Util.Timer              (Timer)
import           Pos.WorkMode.Class          (WorkMode)

----------------------------------------------------------------------------
-- All workers
----------------------------------------------------------------------------

-- | All workers specific to block processing.
blkWorkers
    :: WorkMode ctx m
    => Timer -> ([WorkerSpec m], OutSpecs)
blkWorkers keepAliveTimer =
    merge $ [ blkCreatorWorker
            , blkMetricCheckerWorker
            , retrievalWorker keepAliveTimer
            ]
  where
    merge = mconcatPair . map (first pure)

blkMetricCheckerWorker :: WorkMode ctx m => (WorkerSpec m, OutSpecs)
blkMetricCheckerWorker =
    onNewSlotWorker True announceBlockOuts $ \slotId _ ->
        recoveryCommGuard "onNewSlot worker, blkMetricCheckerWorker" $
             metricWorker slotId

----------------------------------------------------------------------------
-- Block creation worker
----------------------------------------------------------------------------

-- TODO [CSL-1606] Using 'fork' here is quite bad, it's a temporary solution.
blkCreatorWorker :: WorkMode ctx m => (WorkerSpec m, OutSpecs)
blkCreatorWorker =
    onNewSlotWorker True announceBlockOuts $ \slotId sendActions ->
        recoveryCommGuard "onNewSlot worker, blkCreatorWorker" $
            void $ fork $
            blockCreator slotId sendActions `catchAny` onBlockCreatorException
  where
    onBlockCreatorException = reportOrLogE "blockCreator failed: "


blockCreator
    :: WorkMode ctx m
    => SlotId -> SendActions m -> m ()
blockCreator (slotId@SlotId {..}) sendActions = do

    -- First of all we create genesis block if necessary.
    mGenBlock <- createGenesisBlockAndApply siEpoch
    whenJust mGenBlock $ \createdBlk -> do
        logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
        jsonLog $ jlCreatedBlock (Left createdBlk)

    -- Then we get leaders for current epoch.
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
        let validLightCert = find (\psk -> addressHash (pskIssuerPk psk) == leader &&
                                           pskDelegatePk psk == ourPk)
                             validCerts
        let ourLightPsk = find (\psk -> pskIssuerPk psk == ourPk) validCerts
        let lightWeDelegated = isJust ourLightPsk
        let lightWeAreDelegate = isJust validLightCert
        logDebugS $ sformat ("Available to use lightweight PSKs: "%listJson) validCerts

        ourHeavyPsk <- getPskByIssuer (Left ourPk)
        let heavyWeAreIssuer = isJust ourHeavyPsk
        dlgTransM <- getDlgTransPsk leader
        let finalHeavyPsk = snd <$> dlgTransM
        logDebug $ "End delegation psk for this slot: " <> maybe "none" pretty finalHeavyPsk
        let heavyWeAreDelegate = maybe False ((== ourPk) . pskDelegatePk) finalHeavyPsk

        let weAreLeader = leader == ourPkHash
        if | weAreLeader && heavyWeAreIssuer ->
                 logInfoS $ sformat
                 ("Not creating the block (though we're leader) because it's "%
                  "delegated by heavy psk: "%build)
                 ourHeavyPsk
           | weAreLeader && lightWeDelegated ->
                 logInfoS $ sformat
                 ("Not creating the block (though we're leader) because it's "%
                  "delegated by light psk: "%build)
                 ourLightPsk
           | weAreLeader ->
                 onNewSlotWhenLeader slotId Nothing sendActions
           | heavyWeAreDelegate ->
                 let pske = Right . swap <$> dlgTransM
                 in onNewSlotWhenLeader slotId pske sendActions
           | lightWeAreDelegate ->
                 onNewSlotWhenLeader slotId  (Left <$> validLightCert) sendActions
           | otherwise -> pass

onNewSlotWhenLeader
    :: WorkMode ctx m
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
-- Metric worker
----------------------------------------------------------------------------

-- This worker computes various metrics and records them using 'MetricMonitor'.
--
-- Chain quality checker should be called only when we are
-- synchronized with the network.  The goal of this checker is not to
-- detect violation of chain quality assumption, but to produce
-- warnings when this assumption is close to being violated.
--
-- Apart from chain quality check we also record some generally useful values.
metricWorker
    :: forall ctx m. WorkMode ctx m
    => SlotId -> m ()
metricWorker curSlot = do
    OldestFirst lastSlots <- slogGetLastSlots
    reportTotalBlocks
    reportSlottingData curSlot
    reportCrucialValues
    -- If total number of blocks is less than `blkSecurityParam' we do
    -- nothing with regards to chain quality for two reasons:
    -- 1. Usually after we deploy cluster we monitor it manually for a while.
    -- 2. Sometimes we deploy after start time, so chain quality may indeed by
    --    poor right after launch.
    case nonEmpty lastSlots of
        Nothing -> pass
        Just slotsNE
            | length slotsNE < fromIntegral blkSecurityParam -> pass
            | otherwise -> chainQualityChecker curSlot (NE.head slotsNE)

----------------------------------------------------------------------------
-- -- General metrics
----------------------------------------------------------------------------

reportTotalBlocks ::
       forall ctx m. WorkMode ctx m
    => m ()
reportTotalBlocks = do
    difficulty <- view difficultyL <$> DB.getTipHeader
    monitor <- difficultyMonitor <$> view scDifficultyMonitorState
    recordValue monitor difficulty

-- We don't need debug messages, we can see it from other messages.
difficultyMonitor ::
       MetricMonitorState ChainDifficulty -> MetricMonitor ChainDifficulty
difficultyMonitor = noReportMonitor fromIntegral Nothing

reportSlottingData :: WorkMode ctx m => SlotId -> m ()
reportSlottingData slotId = do
    -- epoch
    let epoch = siEpoch slotId
    epochMonitor <-
        noReportMonitor fromIntegral Nothing <$> view scEpochMonitorState
    recordValue epochMonitor epoch
    -- local slot
    let localSlot = siSlot slotId
    localSlotMonitor <-
        noReportMonitor (fromIntegral . getSlotIndex) Nothing <$>
        view scLocalSlotMonitorState
    recordValue localSlotMonitor localSlot
    -- global slot
    let globalSlot = flattenSlotId slotId
    globalSlotMonitor <-
        noReportMonitor fromIntegral Nothing <$>
        view scGlobalSlotMonitorState
    recordValue globalSlotMonitor globalSlot

reportCrucialValues :: WorkMode ctx m => m ()
reportCrucialValues = do
    label <- view scCrucialValuesLabel
    BlockVersionData {..} <- getAdoptedBVData
    let slotDur = bvdSlotDuration
    let epochDur = fromIntegral epochSlots * slotDur
    let crucialValuesText =
            sformat crucialValuesFmt slotDur epochDur blkSecurityParam
    liftIO $ Label.set label crucialValuesText
  where
    crucialValuesFmt =
        "slot duration: " %build % ", epoch duration: " %build % ", k: " %int

----------------------------------------------------------------------------
-- -- Chain quality
----------------------------------------------------------------------------

chainQualityChecker ::
       forall ctx m. WorkMode ctx m
    => SlotId
    -> FlatSlotId
    -> m ()
chainQualityChecker curSlot kThSlot = do
    logDebug $ sformat ("Block with depth 'k' ("%int%
                        ") was created during slot "%slotIdF)
        blkSecurityParam (unflattenSlotId kThSlot)
    let curFlatSlot = flattenSlotId curSlot
    isBootstrapEra <- gsIsBootstrapEra (siEpoch curSlot)
    monitorStateK <- view scCQkMonitorState
    let monitorK = cqkMetricMonitor monitorStateK isBootstrapEra
    monitorOverall <- cqOverallMetricMonitor <$> view scCQOverallMonitorState
    monitorFixed <- cqFixedMetricMonitor <$> view scCQFixedMonitorState
    whenJustM (calcChainQualityM curFlatSlot) (recordValue monitorK)
    whenJustM calcOverallChainQuality $ recordValue monitorOverall
    whenJustM calcChainQualityFixedTime $ recordValue monitorFixed

-- Monitor for chain quality for last k blocks.
cqkMetricMonitor ::
       HasConfiguration
    => MetricMonitorState Double
    -> Bool
    -> MetricMonitor Double
cqkMetricMonitor st isBootstrapEra =
    MetricMonitor
    { mmState = st
    , mmReportMisbehaviour = classifier
    , mmMisbehFormat =
          "Poor chain quality for the last 'k' ("%kFormat%") blocks, "%
          "less than "%now (bprint cqF criticalThreshold)%": "%cqF
    , mmDebugFormat =
          Just $ "Chain quality for the last 'k' ("%kFormat% ") blocks is "%cqF
    , mmConvertValue = convertCQ
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

cqOverallMetricMonitor :: MetricMonitorState Double -> MetricMonitor Double
cqOverallMetricMonitor = noReportMonitor convertCQ (Just debugFormat)
  where
    debugFormat = "Overall chain quality is " %cqF

cqFixedMetricMonitor :: HasConfiguration => MetricMonitorState Double -> MetricMonitor Double
cqFixedMetricMonitor = noReportMonitor convertCQ (Just debugFormat)
  where
    debugFormat =
        "Chain quality for last "%now (bprint build fixedTimeCQSec)%
        " is "%cqF

-- Private chain quality formatter.
cqF :: Format r (Double -> r)
cqF = fixed 3

-- Private converter to integer.
convertCQ :: Double -> Int64
convertCQ = round . (* 100)
