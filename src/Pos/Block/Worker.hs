{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block processing related workers.

module Pos.Block.Worker
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens                (ix)
import           Formatting                  (bprint, build, sformat, shown, (%))
import           Mockable                    (delay, fork)
import           Serokell.Util               (listJson, pairF)
import           System.Wlog                 (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (createGenesisBlock, createMainBlock)
import           Pos.Block.Network.Announce  (announceBlock, announceBlockOuts)
import           Pos.Block.Network.Retrieval (retrievalWorker)
import           Pos.Communication.Protocol  (NodeId, OutSpecs, SendActions, Worker',
                                              WorkerSpec, onNewSlotWorker)
import           Pos.Constants               (networkDiameter)
import           Pos.Context                 (getNodeContext, ncPublicKey)
import           Pos.Core.Address            (addressHash)
import           Pos.Crypto                  (ProxySecretKey (pskDelegatePk, pskIssuerPk, pskOmega))
import           Pos.DB.Class                (MonadDBCore)
import           Pos.DB.GState               (getPSKByIssuerAddressHash)
import           Pos.DB.Misc                 (getProxySecretKeys)
import           Pos.Lrc.DB                  (getLeaders)
import           Pos.Slotting                (currentTimeSlotting,
                                              getSlotStartEmpatically)
import           Pos.Ssc.Class               (SscWorkersClass)
import           Pos.Types                   (ProxySKEither, SlotId (..),
                                              Timestamp (Timestamp), gbHeader, slotIdF)
import           Pos.Util                    (logWarningSWaitLinear, mconcatPair)
import           Pos.Util.JsonLog            (jlCreatedBlock, jlLog)
import           Pos.Util.LogSafe            (logDebugS, logInfoS, logNoticeS,
                                              logWarningS)
import           Pos.WorkMode                (WorkMode)
#if defined(WITH_WALLET)
import           Data.Time.Units             (Second, convertUnit)
import           Pos.Block.Network           (requestTipOuts, triggerRecovery)
import           Pos.Communication           (worker)
import           Pos.Slotting                (getLastKnownSlotDuration)
#endif

-- | All workers specific to block processing.
blkWorkers
    :: (MonadDBCore m, SscWorkersClass ssc, WorkMode ssc m)
    => m (Set NodeId)
    -> ([WorkerSpec m], OutSpecs)
blkWorkers getPeers =
    merge $ [ blkOnNewSlot getPeers
            , retrievalWorker getPeers
            ]
#if defined(WITH_WALLET)
            ++ [ queryBlocksWorker getPeers ]
#endif
  where
    merge = mconcatPair . map (first pure)

-- Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode ssc m => m (Set NodeId) -> (WorkerSpec m, OutSpecs)
blkOnNewSlot getPeers = onNewSlotWorker getPeers True announceBlockOuts (blkOnNewSlotImpl getPeers)

blkOnNewSlotImpl :: WorkMode ssc m =>
                    m (Set NodeId) -> SlotId -> SendActions m -> m ()
blkOnNewSlotImpl getPeers (slotId@SlotId {..}) sendActions = do

    -- First of all we create genesis block if necessary.
    mGenBlock <- createGenesisBlock getPeers siEpoch
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
                 onNewSlotWhenLeader getPeers slotId Nothing sendActions
           | heavyWeAreDelegate ->
                 onNewSlotWhenLeader getPeers slotId (Right <$> heavyPskM) sendActions
           | isJust validCert ->
                 onNewSlotWhenLeader getPeers slotId  (Left <$> validCert) sendActions
           | otherwise -> pass

onNewSlotWhenLeader
    :: WorkMode ssc m
    => m (Set NodeId)
    -> SlotId
    -> Maybe ProxySKEither
    -> Worker' m
onNewSlotWhenLeader getPeers slotId pSk sendActions = do
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
    logWarningSWaitLinear 8 "onNewSlotWhenLeader" onNewSlotWhenLeaderDo
  where
    onNewSlotWhenLeaderDo = do
        logInfoS "It's time to create a block for current slot"
        createdBlock <- createMainBlock getPeers slotId pSk
        either whenNotCreated whenCreated createdBlock
        logInfoS "onNewSlotWhenLeader: done"
    whenCreated createdBlk = do
            logInfoS $
                sformat ("Created a new block:\n" %build) createdBlk
            jlLog $ jlCreatedBlock (Right createdBlk)
            void $ fork $ announceBlock getPeers sendActions $ createdBlk ^. gbHeader
    whenNotCreated = logWarningS . (mappend "I couldn't create a new block: ")

#if defined(WITH_WALLET)
-- | When we're behind NAT, other nodes can't send data to us and thus we
-- won't get blocks that are broadcast through the network – we have to reach
-- out to other nodes by ourselves.
--
-- This worker just triggers every @max (slotDur / 4) 5@ seconds and asks for
-- current tip. Does nothing when recovery is enabled.
--
-- FIXME there is a better way. Establish a long-running connection to every
-- peer asking them to push new data on it. This works even for NAT, since it's
-- the consumer which initiates contact.
queryBlocksWorker
    :: (WorkMode ssc m, SscWorkersClass ssc)
    => m (Set NodeId) -> (WorkerSpec m, OutSpecs)
queryBlocksWorker getPeers = worker requestTipOuts $ \sendActions -> do
    slotDur <- getLastKnownSlotDuration
    let delayInterval = max (slotDur `div` 4) (convertUnit $ (5 :: Second))
        action = forever $ do
            logInfo "Querying blocks from behind NAT"
            triggerRecovery getPeers sendActions
            delay $ delayInterval
        handler (e :: SomeException) = do
            logWarning $ "Exception arised in queryBlocksWorker: " <> show e
            delay $ delayInterval * 2
            action `catch` handler
    action `catch` handler
#endif
