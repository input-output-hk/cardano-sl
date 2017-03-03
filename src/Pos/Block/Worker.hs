{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block processing related workers.

module Pos.Block.Worker
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens                (ix)
import           Data.Default                (def)
import           Formatting                  (bprint, build, sformat, shown, (%))
import           Mockable                    (delay, fork)
import           Paths_cardano_sl            (version)
import           Pos.Communication.Protocol  (SendActions)
import           Serokell.Util               (VerificationRes (..), listJson, pairF, sec)
import           System.Wlog                 (WithLogger, logDebug, logError, logInfo,
                                              logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (createGenesisBlock, createMainBlock)
import           Pos.Block.Network.Announce  (announceBlock, announceBlockOuts)
import           Pos.Block.Network.Retrieval (requestTipOuts, retrievalWorker,
                                              triggerRecovery)
import           Pos.Communication.Protocol  (OutSpecs, Worker', WorkerSpec,
                                              onNewSlotWorker, worker)
import           Pos.Constants               (networkDiameter)
import           Pos.Context                 (getNodeContext, isRecoveryMode, ncPublicKey)
import           Pos.Core.Address            (addressHash)
import           Pos.Crypto                  (ProxySecretKey (pskDelegatePk, pskIssuerPk, pskOmega))
import           Pos.DB.GState               (getPSKByIssuerAddressHash)
import           Pos.DB.Misc                 (getProxySecretKeys)
import           Pos.Exception               (assertionFailed)
import           Pos.Lrc.DB                  (getLeaders)
import           Pos.Reporting.Methods       (reportingFatal)
import           Pos.Slotting                (currentTimeSlotting, getCurrentSlot,
                                              getSlotStartEmpatically)
#if !defined(DEV_MODE) && defined(WITH_WALLET)
import           Data.Time.Units             (convertUnit)
import           Pos.Slotting                (getLastKnownSlotDuration)
#endif
import           Pos.Ssc.Class               (SscHelpersClass, SscWorkersClass)
import           Pos.Types                   (MainBlock, ProxySKEither, SlotId (..),
                                              Timestamp (Timestamp),
                                              VerifyBlockParams (..), gbHeader, slotIdF,
                                              verifyBlock)
import           Pos.Util                    (inAssertMode, logWarningWaitLinear,
                                              mconcatPair)
import           Pos.Util.JsonLog            (jlCreatedBlock, jlLog)
import           Pos.WorkMode                (WorkMode)


-- | All workers specific to block processing.
blkWorkers :: (SscWorkersClass ssc, WorkMode ssc m) => ([WorkerSpec m], OutSpecs)
blkWorkers =
    merge [ blkOnNewSlot
          , retrievalWorker
          , recoveryWorker
#if !defined(DEV_MODE) && defined(WITH_WALLET)
          , behindNatWorker
#endif
          ]
  where
    merge = mconcatPair . map (first pure)

-- Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode ssc m => (WorkerSpec m, OutSpecs)
blkOnNewSlot = onNewSlotWorker True announceBlockOuts blkOnNewSlotImpl

blkOnNewSlotImpl :: WorkMode ssc m =>
                    SlotId -> SendActions m -> m ()
blkOnNewSlotImpl (slotId@SlotId {..}) sendActions = do
    -- First of all we create genesis block if necessary.
    mGenBlock <- createGenesisBlock siEpoch
    whenJust mGenBlock $ \createdBlk -> do
        logInfo $ sformat ("Created genesis block:\n" %build) createdBlk
        jlLog $ jlCreatedBlock (Left createdBlk)

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
                  (leaders ^? ix (fromIntegral siSlot))
  where
    onNoLeader =
        logWarning "Couldn't find a leader for current slot among known ones"
    logLeadersF = if siSlot == 0 then logInfo else logDebug
    onKnownLeader leaders leader = do
        ourPk <- ncPublicKey <$> getNodeContext
        let ourPkHash = addressHash ourPk
        proxyCerts <- getProxySecretKeys
        let validCerts =
                filter (\pSk -> let (w0,w1) = pskOmega pSk
                                in siEpoch >= w0 && siEpoch <= w1) proxyCerts
            validCert = find (\pSk -> addressHash (pskIssuerPk pSk) == leader)
                             validCerts
        logLeadersF $ sformat ("Our pk: "%build%", our pkHash: "%build) ourPk ourPkHash
        logLeadersF $ sformat ("Slot leaders: "%listJson) $
                      map (bprint pairF) (zip [0 :: Int ..] $ toList leaders)
        logLeadersF $ sformat ("Current slot leader: "%build) leader
        logDebug $ sformat ("Available to use lightweight PSKs: "%listJson) validCerts
        heavyPskM <- getPSKByIssuerAddressHash leader
        logDebug $ "Does someone have cert for this slot: " <> show (isJust heavyPskM)
        let heavyWeAreDelegate = maybe False ((== ourPk) . pskDelegatePk) heavyPskM
        let heavyWeAreIssuer = maybe False ((== ourPk) . pskIssuerPk) heavyPskM
        if | heavyWeAreIssuer ->
                 logDebug $ sformat
                 ("Not creating the block because it's delegated by psk: "%build)
                 heavyPskM
           | leader == ourPkHash ->
                 onNewSlotWhenLeader slotId Nothing sendActions
           | heavyWeAreDelegate ->
                 onNewSlotWhenLeader slotId (Right <$> heavyPskM) sendActions
           | isJust validCert ->
                 onNewSlotWhenLeader slotId  (Left <$> validCert) sendActions
           | otherwise -> pass

onNewSlotWhenLeader
    :: WorkMode ssc m
    => SlotId
    -> Maybe ProxySKEither
    -> Worker' m
onNewSlotWhenLeader slotId pSk sendActions = do
    let logReason =
            sformat ("I have a right to create a block for the slot "%slotIdF%" ")
                    slotId
        logLeader = "because i'm a leader"
        logCert (Left psk) =
            sformat ("using ligtweight proxy signature key "%build%", will do it soon") psk
        logCert (Right psk) =
            sformat ("using heavyweight proxy signature key "%build%", will do it soon") psk
    logInfo $ logReason <> maybe logLeader logCert pSk
    nextSlotStart <- getSlotStartEmpatically (succ slotId)
    currentTime <- currentTimeSlotting
    let timeToCreate =
            max currentTime (nextSlotStart - Timestamp networkDiameter)
        Timestamp timeToWait = timeToCreate - currentTime
    logInfo $
        sformat ("Waiting for "%shown%" before creating block") timeToWait
    delay timeToWait
    let onNewSlotWhenLeaderDo = do
            logInfo "It's time to create a block for current slot"
            let whenCreated createdBlk = do
                    logInfo $
                        sformat ("Created a new block:\n" %build) createdBlk
                    jlLog $ jlCreatedBlock (Right createdBlk)
                    verifyCreatedBlock createdBlk
                    void $ fork $ announceBlock sendActions $ createdBlk ^. gbHeader
            let whenNotCreated = logWarning . (mappend "I couldn't create a new block: ")
            createdBlock <- createMainBlock slotId pSk
            either whenNotCreated whenCreated createdBlock
            logInfo "onNewSlotWhenLeader: done"
    logWarningWaitLinear 8 "onNewSlotWhenLeader" onNewSlotWhenLeaderDo

verifyCreatedBlock
    :: (WithLogger m, SscHelpersClass ssc, MonadThrow m)
    => MainBlock ssc -> m ()
verifyCreatedBlock blk =
    inAssertMode $
    case verifyBlock vbp (Right blk) of
        VerSuccess -> pass
        VerFailure errors ->
            assertionFailed $
            sformat ("New block failed some checks: " %listJson) errors
  where
    vbp =
        def
        { vbpVerifyGeneric = True
        , vbpVerifySsc = True
        }

-- | Trigger recovery if we're definitely late in the blockchain (for >epoch).
recoveryWorker :: WorkMode ssc m => (WorkerSpec m, OutSpecs)
recoveryWorker = worker requestTipOuts recoveryWorkerImpl

recoveryWorkerImpl
    :: WorkMode ssc m
    => SendActions m -> m ()
recoveryWorkerImpl sendActions = action `catch` handler
  where
    action = reportingFatal version $ forever $ checkRecovery >> delay (sec 5)
    checkRecovery = do
        recMode <- isRecoveryMode
        curSlotNothing <- isNothing <$> getCurrentSlot
        if (curSlotNothing && not recMode) then do
             logDebug "Recovery worker: don't know current slot"
             triggerRecovery sendActions
        else logDebug $ "Recovery worker skipped:" <>
                        " slot is nothing: " <> show curSlotNothing <>
                        ", recovery mode is on: " <> show recMode

    handler (e :: SomeException) = do
        logError $ "Error happened in recoveryWorker: " <> show e
        delay (sec 10)
        action `catch` handler

#if !defined(DEV_MODE) && defined(WITH_WALLET)
-- | This one just triggers every @max (slotDur / 4) 5@ seconds and
-- asks for current tip. Does nothing when recovery is enabled.
behindNatWorker :: WorkMode ssc m => (WorkerSpec m, OutSpecs)
behindNatWorker = worker requestTipOuts $ \sendActions -> do
    slotDur <- getLastKnownSlotDuration
    let delayInterval = max (slotDur `div` 4) (convertUnit $ sec 5)
        action = forever $ do
            triggerRecovery sendActions
            delay $ delayInterval
        handler (e :: SomeException) = do
            logWarning $ "Exception arised in behindNatWorker: " <> show e
            delay $ delayInterval * 2
            action `catch` handler
    action `catch` handler
#endif
