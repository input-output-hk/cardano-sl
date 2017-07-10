{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block processing related workers.

module Pos.Block.Worker
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Universum

import           Control.Lens                (ix)
import           Formatting                  (bprint, build, sformat, shown, (%))
import           Mockable                    (delay, fork)
import           Serokell.Util               (listJson, pairF, sec)
import           System.Wlog                 (logDebug, logInfo, logWarning)

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (createGenesisBlock, createMainBlock)
import           Pos.Block.Network.Announce  (announceBlock, announceBlockOuts)
import           Pos.Block.Network.Retrieval (retrievalWorker)
import           Pos.Communication.Protocol  (OutSpecs, SendActions, Worker', WorkerSpec,
                                              onNewSlotWorker)
import           Pos.Constants               (networkDiameter)
import           Pos.Context                 (getOurPublicKey, recoveryCommGuard)
import           Pos.Core                    (SlotId (..), Timestamp (Timestamp),
                                              gbHeader, getSlotIndex, slotIdF)
import           Pos.Core.Address            (addressHash)
import           Pos.Crypto                  (ProxySecretKey (pskDelegatePk, pskIssuerPk, pskOmega))
import           Pos.DB.GState               (getPskByIssuer)
import           Pos.DB.Misc                 (getProxySecretKeys)
import           Pos.Delegation.Helpers      (isRevokePsk)
import           Pos.Delegation.Logic        (getDlgTransPsk)
import           Pos.Delegation.Types        (ProxySKBlockInfo)
import           Pos.Lrc.DB                  (getLeaders)
import           Pos.Slotting                (currentTimeSlotting,
                                              getSlotStartEmpatically)
import           Pos.Ssc.Class               (SscWorkersClass)
import           Pos.Util                    (logWarningSWaitLinear, mconcatPair)
import           Pos.Util.JsonLog            (jlCreatedBlock)
import           Pos.Util.LogSafe            (logDebugS, logInfoS, logWarningS)
import           Pos.Util.TimeWarp           (CanJsonLog (..))
import           Pos.WorkMode.Class          (WorkMode)
#if defined(WITH_WALLET)
import           Data.Time.Units             (Second, convertUnit)
import           Pos.Block.Network           (requestTipOuts, triggerRecovery)
import           Pos.Communication           (worker)
import           Pos.Slotting                (getLastKnownSlotDuration)
#endif

-- | All workers specific to block processing.
blkWorkers
    :: (SscWorkersClass ssc, WorkMode ssc ctx m)
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
blkOnNewSlot :: WorkMode ssc ctx m => (WorkerSpec m, OutSpecs)
blkOnNewSlot =
    onNewSlotWorker True announceBlockOuts $ \slotId sendActions ->
        recoveryCommGuard $ blkOnNewSlotImpl slotId sendActions

blkOnNewSlotImpl
    :: WorkMode ssc ctx m
    => SlotId -> SendActions m -> m ()
blkOnNewSlotImpl (slotId@SlotId {..}) sendActions = do

    -- First of all we create genesis block if necessary.
    mGenBlock <- createGenesisBlock siEpoch
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

        proxyCerts <- getProxySecretKeys -- TODO rename it with "light" suffix
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
    -> Worker' m
onNewSlotWhenLeader slotId pske sendActions = do
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
        createdBlock <- createMainBlock slotId pske
        either whenNotCreated whenCreated createdBlock
        logInfoS "onNewSlotWhenLeader: done"
    whenCreated createdBlk = do
            logInfoS $
                sformat ("Created a new block:\n" %build) createdBlk
            jsonLog $ jlCreatedBlock (Right createdBlk)
            void $ fork $ announceBlock sendActions $ createdBlk ^. gbHeader
    whenNotCreated = logWarningS . (mappend "I couldn't create a new block: ")

#if defined(WITH_WALLET)
-- | When we're behind NAT, other nodes can't send data to us and thus we
-- won't get blocks that are broadcast through the network – we have to reach
-- out to other nodes by ourselves.
--
-- This worker just triggers every @max (slotDur / 4) 5@ seconds and asks for
-- current tip. Does nothing when recovery is enabled, because then we're
-- receiving blocks anyway.
--
-- FIXME there is a better way. Establish a long-running connection to every
-- peer asking them to push new data on it. This works even for NAT, since it's
-- the consumer which initiates contact.
queryBlocksWorker
    :: (WorkMode ssc ctx m, SscWorkersClass ssc)
    => (WorkerSpec m, OutSpecs)
queryBlocksWorker = worker requestTipOuts $ \sendActions -> do
    let action = forever $ do
            slotDur <- getLastKnownSlotDuration
            let delayInterval = max (slotDur `div` 4) (convertUnit $ (5 :: Second))
            recoveryCommGuard $ do
                logInfo "Querying blocks from behind NAT"
                triggerRecovery sendActions
            delay $ delayInterval
        handler (e :: SomeException) = do
            logWarning $ "Exception arised in queryBlocksWorker: " <> show e
             -- getLastKnownSlotDuration may fail, so we'll just wait
             -- arbitrary number of seconds (instead of e.g. slotDur / 4)
            delay $ sec 4
            action `catch` handler
    afterDelay $ action `catch` handler
  where
    afterDelay action = delay (sec 3) >> action
#endif
