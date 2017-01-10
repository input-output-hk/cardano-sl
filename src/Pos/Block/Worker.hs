{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Block processing related workers.

module Pos.Block.Worker
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens               (ix, (^.), (^?))
import           Control.TimeWarp.Timed     (for, wait)
import           Data.Default               (def)
import           Formatting                 (build, sformat, shown, (%))
import           Serokell.Util              (VerificationRes (..), listJson)
import           Serokell.Util.Exceptions   ()
import           System.Wlog                (WithLogger, logDebug, logError, logInfo,
                                             logWarning)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic            (createGenesisBlock, createMainBlock)
import           Pos.Block.Network.Announce (announceBlock)
import           Pos.Constants              (networkDiameter)
import           Pos.Context                (getNodeContext, ncPublicKey)
import           Pos.Crypto                 (ProxySecretKey (pskDelegatePk, pskIssuerPk, pskOmega),
                                             shortHashF)
import           Pos.DB.GState              (getPSKByIssuerAddressHash)
import           Pos.DB.Lrc                 (getLeaders)
import           Pos.DB.Misc                (getProxySecretKeys)
import           Pos.Slotting               (MonadSlots (getCurrentTime), getSlotStart,
                                             onNewSlot)
import           Pos.Ssc.Class              (SscHelpersClass)
import           Pos.Types                  (MainBlock, ProxySKEither, SlotId (..),
                                             Timestamp (Timestamp),
                                             VerifyBlockParams (..), gbHeader, slotIdF,
                                             verifyBlock)
import           Pos.Types.Address          (addressHash)
import           Pos.Util                   (inAssertMode, logWarningWaitLinear)
import           Pos.Util.JsonLog           (jlCreatedBlock, jlLog)
import           Pos.WorkMode               (WorkMode)

-- | All workers specific to block processing.
blkWorkers :: WorkMode ssc m => [m ()]
blkWorkers = [blkOnNewSlotWorker]

blkOnNewSlotWorker :: WorkMode ssc m => m ()
blkOnNewSlotWorker = onNewSlot True blkOnNewSlot

-- Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode ssc m => SlotId -> m ()
blkOnNewSlot slotId@SlotId {..} = do
    -- First of all we create genesis block if necessary.
    mGenBlock <- createGenesisBlock slotId
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
        logLeadersF $ sformat ("Slot leaders: "%listJson) $ map (sformat shortHashF) leaders
        logLeadersF $ sformat ("Current slot leader: "%build) leader
        logDebug $ sformat ("Available lightweight PSKs: "%listJson) validCerts
        heavyPskM <- getPSKByIssuerAddressHash leader
        logDebug $ "Does someone have cert for this slot: " <> show (isJust heavyPskM)
        let heavyWeAreDelegate = maybe False ((== ourPk) . pskDelegatePk) heavyPskM
        let heavyWeAreIssuer = maybe False ((== ourPk) . pskIssuerPk) heavyPskM
        if | heavyWeAreIssuer -> pass
           | leader == ourPkHash -> onNewSlotWhenLeader slotId Nothing
           | heavyWeAreDelegate -> onNewSlotWhenLeader slotId $ Right <$> heavyPskM
           | isJust validCert -> onNewSlotWhenLeader slotId $ Left <$> validCert
           | otherwise -> pass

onNewSlotWhenLeader
    :: WorkMode ssc m
    => SlotId
    -> Maybe ProxySKEither
    -> m ()
onNewSlotWhenLeader slotId pSk = do
    let logReason =
            sformat ("I have a right to create a block for the slot "%slotIdF%" ")
                    slotId
        logLeader = "because i'm a leader"
        logCert (Left psk) =
            sformat ("using ligtweight proxy signature key "%build%", will do it soon") psk
        logCert (Right psk) =
            sformat ("using heavyweight proxy signature key "%build%", will do it soon") psk
    logInfo $ logReason <> maybe logLeader logCert pSk
    nextSlotStart <- getSlotStart (succ slotId)
    currentTime <- getCurrentTime
    let timeToCreate =
            max currentTime (nextSlotStart - Timestamp networkDiameter)
        Timestamp timeToWait = timeToCreate - currentTime
    logInfo $
        sformat ("Waiting for "%shown%" before creating block") timeToWait
    wait (for timeToWait)
    let onNewSlotWhenLeaderDo = do
            logInfo "It's time to create a block for current slot"
            let whenCreated createdBlk = do
                    logInfo $
                        sformat ("Created a new block:\n" %build) createdBlk
                    jlLog $ jlCreatedBlock (Right createdBlk)
                    verifyCreatedBlock createdBlk
                    announceBlock $ createdBlk ^. gbHeader
            let whenNotCreated = logWarning . (mappend "I couldn't create a new block: ")
            createdBlock <- createMainBlock slotId pSk
            either whenNotCreated whenCreated createdBlock
    logWarningWaitLinear 8 "onNewSlotWhenLeader" onNewSlotWhenLeaderDo

verifyCreatedBlock :: (WithLogger m, SscHelpersClass ssc) => MainBlock ssc -> m ()
verifyCreatedBlock blk =
    inAssertMode $
    case verifyBlock vbp (Right blk) of
        VerSuccess -> pass
        VerFailure errors ->
            logError $ sformat ("New block failed some checks: " %listJson) errors
  where
    vbp =
        def
        { vbpVerifyGeneric = True
        , vbpVerifyTxs = True
        , vbpVerifySsc = True
        }
