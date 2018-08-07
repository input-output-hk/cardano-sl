{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Block processing related workers.

module Pos.Worker.Block
       ( blkWorkers
       ) where


import           Universum

import           Control.Lens (ix)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Microsecond, Second, fromMicroseconds)
import           Formatting (Format, bprint, build, fixed, int, now, sformat,
                     shown, (%))
import           Serokell.Util (enumerate, listJson, pairF)
import qualified System.Metrics.Label as Label
import           System.Random (randomRIO)
import           System.Wlog (logDebug, logError, logInfo, logWarning)

import           Pos.Chain.Block (HasBlockConfiguration, criticalCQ,
                     criticalCQBootstrap, fixedTimeCQSec, networkDiameter,
                     nonCriticalCQ, nonCriticalCQBootstrap,
                     scCQFixedMonitorState, scCQOverallMonitorState,
                     scCQkMonitorState, scCrucialValuesLabel,
                     scDifficultyMonitorState, scEpochMonitorState,
                     scGlobalSlotMonitorState, scLocalSlotMonitorState)
import           Pos.Chain.Delegation (ProxySKBlockInfo)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core (ChainDifficulty, FlatSlotId, HasProtocolConstants,
                     SlotId (..), Timestamp (Timestamp), addressHash,
                     blkSecurityParam, difficultyL, epochOrSlotToSlot,
                     epochSlots, flattenSlotId, getEpochOrSlot,
                     getOurPublicKey, getSlotIndex, slotIdF, unflattenSlotId)
import           Pos.Core.Block (gbHeader)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Conc (delay)
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.Core.Reporting (HasMisbehaviorMetrics, MetricMonitor (..),
                     MetricMonitorState, noReportMonitor, recordValue)
import           Pos.Core.Update (BlockVersionData (..))
import           Pos.Crypto (ProtocolMagic, ProxySecretKey (pskDelegatePk))
import           Pos.DB (gsIsBootstrapEra)
import           Pos.DB.Block (calcChainQualityFixedTime, calcChainQualityM,
                     calcOverallChainQuality, createGenesisBlockAndApply,
                     createMainBlockAndApply, slogGetLastSlots)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Delegation (getDlgTransPsk, getPskByIssuer)
import qualified Pos.DB.Lrc as LrcDB (getLeadersForEpoch)
import           Pos.DB.Update (getAdoptedBVData)
import           Pos.Infra.Diffusion.Types (Diffusion)
import qualified Pos.Infra.Diffusion.Types as Diffusion
                     (Diffusion (announceBlockHeader))
import           Pos.Infra.Recovery.Info (getSyncStatus, getSyncStatusK,
                     needTriggerRecovery, recoveryCommGuard)
import           Pos.Infra.Reporting (reportOrLogE)
import           Pos.Infra.Slotting (ActionTerminationPolicy (..),
                     OnNewSlotParams (..), currentTimeSlotting,
                     defaultOnNewSlotParams, getSlotStartEmpatically,
                     onNewSlot)
import           Pos.Infra.Util.JsonLog.Events (jlCreatedBlock)
import           Pos.Infra.Util.LogSafe (logDebugS, logInfoS, logWarningS)
import           Pos.Infra.Util.TimeLimit (logWarningSWaitLinear)
import           Pos.Network.Block.Logic (triggerRecovery)
import           Pos.Network.Block.Retrieval (retrievalWorker)
import           Pos.Network.Block.WorkMode (BlockWorkMode)

----------------------------------------------------------------------------
-- All workers
----------------------------------------------------------------------------

-- | All workers specific to block processing.
blkWorkers
    :: ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => ProtocolMagic
    -> TxpConfiguration
    -> [Diffusion m -> m ()]
blkWorkers pm txpConfig =
    [ blkCreatorWorker pm txpConfig
    , informerWorker
    , retrievalWorker pm txpConfig
    , recoveryTriggerWorker pm
    ]

informerWorker
    :: ( BlockWorkMode ctx m
    ) => Diffusion m -> m ()
informerWorker =
    \_ -> onNewSlot defaultOnNewSlotParams $ \slotId ->
        recoveryCommGuard "onNewSlot worker, informerWorker" $ do
            tipHeader <- DB.getTipHeader
            -- Printe tip header
            logDebug $ sformat ("Our tip header: "%build) tipHeader
            -- Print the difference between tip slot and current slot.
            logHowManySlotsBehind slotId tipHeader
            -- Compute and report metrics
            metricWorker slotId
  where
    logHowManySlotsBehind slotId tipHeader =
        let tipSlot = epochOrSlotToSlot (getEpochOrSlot tipHeader)
            slotDiff = flattenSlotId slotId - flattenSlotId tipSlot
        in logInfo $ sformat ("Difference between current slot and tip slot is: "
                              %int) slotDiff


----------------------------------------------------------------------------
-- Block creation worker
----------------------------------------------------------------------------

blkCreatorWorker
    :: ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => ProtocolMagic
    -> TxpConfiguration
    -> Diffusion m -> m ()
blkCreatorWorker pm txpConfig =
    \diffusion -> onNewSlot onsp $ \slotId ->
        recoveryCommGuard "onNewSlot worker, blkCreatorWorker" $
        blockCreator pm txpConfig slotId diffusion `catchAny` onBlockCreatorException
  where
    onBlockCreatorException = reportOrLogE "blockCreator failed: "
    onsp :: OnNewSlotParams
    onsp =
        defaultOnNewSlotParams
        {onspTerminationPolicy = NewSlotTerminationPolicy "block creator"}

blockCreator
    :: ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => ProtocolMagic
    -> TxpConfiguration
    -> SlotId
    -> Diffusion m -> m ()
blockCreator pm txpConfig (slotId@SlotId {..}) diffusion = do

    -- First of all we create genesis block if necessary.
    mGenBlock <- createGenesisBlockAndApply pm txpConfig siEpoch
    whenJust mGenBlock $ \createdBlk -> do
        logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
        jsonLog $ jlCreatedBlock (Left createdBlk)

    -- Then we get leaders for current epoch.
    leadersMaybe <- LrcDB.getLeadersForEpoch siEpoch
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
        logError "Couldn't find a leader for current slot among known ones"
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
            strLeaders = map (bprint pairF) (enumerate @Int (toList leaders))
        logDebug $ sformat ("Trimmed leaders: "%listJson)
                 $ dropAround (fromEnum siSlot) 10 strLeaders

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
           | weAreLeader ->
                 onNewSlotWhenLeader pm txpConfig slotId Nothing diffusion
           | heavyWeAreDelegate ->
                 let pske = swap <$> dlgTransM
                 in onNewSlotWhenLeader pm txpConfig slotId pske diffusion
           | otherwise -> pass

onNewSlotWhenLeader
    :: ( BlockWorkMode ctx m
       )
    => ProtocolMagic
    -> TxpConfiguration
    -> SlotId
    -> ProxySKBlockInfo
    -> Diffusion m
    -> m ()
onNewSlotWhenLeader pm txpConfig slotId pske diffusion = do
    let logReason =
            sformat ("I have a right to create a block for the slot "%slotIdF%" ")
                    slotId
        logLeader = "because i'm a leader"
        logCert (psk,_) =
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
        createdBlock <- createMainBlockAndApply pm txpConfig slotId pske
        either whenNotCreated whenCreated createdBlock
        logInfoS "onNewSlotWhenLeader: done"
    whenCreated createdBlk = do
            logInfoS $
                sformat ("Created a new block:\n" %build) createdBlk
            jsonLog $ jlCreatedBlock (Right createdBlk)
            void $ Diffusion.announceBlockHeader diffusion $ createdBlk ^. gbHeader
    whenNotCreated = logWarningS . (mappend "I couldn't create a new block: ")

----------------------------------------------------------------------------
-- Recovery trigger worker
----------------------------------------------------------------------------

recoveryTriggerWorker
    :: forall ctx m.
       ( BlockWorkMode ctx m
       )
    => ProtocolMagic -> Diffusion m -> m ()
recoveryTriggerWorker pm diffusion = do
    -- Initial heuristic delay is needed (the system takes some time
    -- to initialize).
    -- TBD why 3 seconds? Why delay at all? Come on, we can do better.
    delay (3 :: Second)

    repeatOnInterval $ do
        doTrigger <- needTriggerRecovery <$> getSyncStatusK
        when doTrigger $ do
            logInfo "Triggering recovery because we need it"
            triggerRecovery pm diffusion

        -- Sometimes we want to trigger recovery just in case. Maybe
        -- we're just 5 slots late, but nobody wants to send us
        -- blocks. It may happen sometimes, because nobody actually
        -- guarantees that node will get updates on time. So we
        -- sometimes ask for tips even if we're in relatively safe
        -- situation.
        (d :: Double) <- liftIO $ randomRIO (0,1)
        -- P = 0.004 ~ every 250th time (250 seconds ~ every 4.2 minutes)
        let triggerSafety = not doTrigger && d < 0.004
        when triggerSafety $ do
            logInfo "Checking if we need recovery as a safety measure"
            whenM (needTriggerRecovery <$> getSyncStatus 5) $ do
                logInfo "Triggering recovery as a safety measure"
                triggerRecovery pm diffusion

        -- We don't want to ask for tips too frequently.
        -- E.g. there may be a tip processing mistake so that we
        -- never go into recovery even though we recieve
        -- headers. Or it may happen that we will receive only
        -- useless broken tips for some reason (attack?). This
        -- will minimize risks and network load.
        when (doTrigger || triggerSafety) $ delay (20 :: Second)
  where
    repeatOnInterval action = void $ do
        delay (1 :: Second)
        -- REPORT:ERROR 'reportOrLogE' in recovery trigger worker
        void $ action `catchAny` \e -> do
            reportOrLogE "recoveryTriggerWorker" e
            delay (15 :: Second)
        repeatOnInterval action

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
    :: BlockWorkMode ctx m
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
       forall ctx m. BlockWorkMode ctx m
    => m ()
reportTotalBlocks = do
    difficulty <- view difficultyL <$> DB.getTipHeader
    monitor <- difficultyMonitor <$> view scDifficultyMonitorState
    recordValue monitor difficulty

-- We don't need debug messages, we can see it from other messages.
difficultyMonitor ::
       MetricMonitorState ChainDifficulty -> MetricMonitor ChainDifficulty
difficultyMonitor = noReportMonitor fromIntegral Nothing

reportSlottingData :: BlockWorkMode ctx m => SlotId -> m ()
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

reportCrucialValues :: BlockWorkMode ctx m => m ()
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
       ( BlockWorkMode ctx m
       )
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
       ( HasBlockConfiguration, HasProtocolConstants )
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
        | not decreased && timePassed < fromMicroseconds 400000000 = Nothing
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

cqFixedMetricMonitor ::
       HasBlockConfiguration
    => MetricMonitorState Double
    -> MetricMonitor Double
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
