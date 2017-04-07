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
import           Serokell.Util               (VerificationRes (..), listJson, pairF)
import           System.Wlog                 (WithLogger, logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (createGenesisBlock, createMainBlock)
import           Pos.Block.Network.Announce  (announceBlock, announceBlockOuts)
import           Pos.Block.Network.Retrieval (retrievalWorker)
import           Pos.Communication.Protocol  (OutSpecs, SendActions, Worker', WorkerSpec,
                                              onNewSlotWorker)
import           Pos.Constants               (networkDiameter)
import           Pos.Context                 (getNodeContext, ncPublicKey)
import           Pos.Core.Address            (addressHash)
import           Pos.Crypto                  (ProxySecretKey (pskDelegatePk, pskIssuerPk, pskOmega))
import           Pos.DB.GState               (getPSKByIssuerAddressHash)
import           Pos.DB.Misc                 (getProxySecretKeys)
import           Pos.Exception               (assertionFailed)
import           Pos.Lrc.DB                  (getLeaders)
import           Pos.Slotting                (currentTimeSlotting,
                                              getSlotStartEmpatically)
#if defined(WITH_WALLET)
import           Data.Time.Units             (Second, convertUnit)
import           Pos.Block.Network           (requestTipOuts, triggerRecovery)
import           Pos.Communication           (worker)
import           Pos.Slotting                (getLastKnownSlotDuration)
#endif
import           Pos.Ssc.Class               (SscHelpersClass, SscWorkersClass)
import           Pos.Types                   (MainBlock, ProxySKEither, SlotId (..),
                                              Timestamp (Timestamp),
                                              VerifyBlockParams (..), gbHeader, slotIdF,
                                              verifyBlock)
import           Pos.Util                    (inAssertMode, logWarningSWaitLinear,
                                              mconcatPair)
import           Pos.Util.JsonLog            (jlCreatedBlock, jlLog)
import           Pos.Util.LogSafe            (logDebugS, logInfoS, logNoticeS,
                                              logWarningS)
import           Pos.WorkMode                (WorkMode)


-- | All workers specific to block processing.
blkWorkers
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => ([WorkerSpec m], OutSpecs)
blkWorkers =
    merge $ [ blkOnNewSlot
            , retrievalWorker
            ]
#if defined(WITH_WALLET)
            ++ [ queryBlocksWorker ]
#endif
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
    logLeadersFS = if siSlot == 0 then logInfoS else logDebugS
    onKnownLeader leaders leader = do
        ourPk <- ncPublicKey <$> getNodeContext
        let ourPkHash = addressHash ourPk
        proxyCerts <- getProxySecretKeys
        let validCerts =
                filter (\pSk -> let (w0,w1) = pskOmega pSk
                                in siEpoch >= w0 && siEpoch <= w1) proxyCerts
            validCert = find (\pSk -> addressHash (pskIssuerPk pSk) == leader)
                             validCerts
        logNoticeS "This is a test debug message which shouldn't be sent to the logging server."
        logLeadersFS $ sformat ("Our pk: "%build%", our pkHash: "%build) ourPk ourPkHash
        logLeadersF $ sformat ("Slot leaders: "%listJson) $
                      map (bprint pairF) (zip [0 :: Int ..] $ toList leaders)
        logLeadersF $ sformat ("Current slot leader: "%build) leader
        logDebugS $ sformat ("Available to use lightweight PSKs: "%listJson) validCerts
        heavyPskM <- getPSKByIssuerAddressHash leader
        logDebug $ "Does someone have cert for this slot: " <> show (isJust heavyPskM)
        let heavyWeAreDelegate = maybe False ((== ourPk) . pskDelegatePk) heavyPskM
        let heavyWeAreIssuer = maybe False ((== ourPk) . pskIssuerPk) heavyPskM
        if | heavyWeAreIssuer ->
                 logDebugS $ sformat
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
    logInfoS $ logReason <> maybe logLeader logCert pSk
    nextSlotStart <- getSlotStartEmpatically (succ slotId)
    currentTime <- currentTimeSlotting
    let timeToCreate =
            max currentTime (nextSlotStart - Timestamp networkDiameter)
        Timestamp timeToWait = timeToCreate - currentTime
    logInfoS $
        sformat ("Waiting for "%shown%" before creating block") timeToWait
    delay timeToWait
    let onNewSlotWhenLeaderDo = do
            logInfoS "It's time to create a block for current slot"
            let whenCreated createdBlk = do
                    logInfoS $
                        sformat ("Created a new block:\n" %build) createdBlk
                    jlLog $ jlCreatedBlock (Right createdBlk)
                    verifyCreatedBlock createdBlk
                    void $ fork $ announceBlock sendActions $ createdBlk ^. gbHeader
            let whenNotCreated = logWarningS . (mappend "I couldn't create a new block: ")
            createdBlock <- createMainBlock slotId pSk
            either whenNotCreated whenCreated createdBlock
            logInfoS "onNewSlotWhenLeader: done"
    logWarningSWaitLinear 8 "onNewSlotWhenLeader" onNewSlotWhenLeaderDo

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

#if defined(WITH_WALLET)
-- | When we're behind NAT, other nodes can't send data to us and thus we
-- won't get blocks that are broadcast through the network – we have to reach
-- out to other nodes by ourselves.
--
-- This worker just triggers every @max (slotDur / 4) 5@ seconds and asks for
-- current tip. Does nothing when recovery is enabled.
queryBlocksWorker
    :: (WorkMode ssc m, SscWorkersClass ssc)
    => (WorkerSpec m, OutSpecs)
queryBlocksWorker = worker requestTipOuts $ \sendActions -> do
    slotDur <- getLastKnownSlotDuration
    let delayInterval = max (slotDur `div` 4) (convertUnit $ (5 :: Second))
        action = forever $ do
            triggerRecovery sendActions
            delay $ delayInterval
        handler (e :: SomeException) = do
            logWarning $ "Exception arised in queryBlocksWorker: " <> show e
            delay $ delayInterval * 2
            action `catch` handler
    action `catch` handler
#endif
