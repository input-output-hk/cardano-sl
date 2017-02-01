{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Concurrent.STM           (readTVar)
import           Control.Lens                     (at, to)
import           Control.Monad.Except             (runExceptT)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import           Data.Tagged                      (Tagged (..))
import           Data.Time.Units                  (Microsecond, Millisecond, convertUnit)
import           Formatting                       (build, ords, sformat, shown, (%))
import           Mockable                         (currentTime, delay)
import           Serokell.Util.Exceptions         ()
import           Serokell.Util.Text               (listJson)
import           System.Wlog                      (logDebug, logError, logInfo,
                                                   logWarning)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Binary.Communication         ()
import           Pos.Binary.Relay                 ()
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Message        ()
import           Pos.Communication.Protocol       (OutSpecs, SendActions, Worker',
                                                   WorkerSpec, onNewSlotWorker, oneMsgH,
                                                   toOutSpecs)
import           Pos.Communication.Relay          (DataMsg (..), InvMsg (..))
import           Pos.Constants                    (mpcSendInterval, slotSecurityParam,
                                                   vssMaxTTL)
import           Pos.Context                      (getNodeContext, lrcActionOnEpochReason,
                                                   ncPublicKey, ncSecretKey, ncSscContext)
import           Pos.Crypto                       (SecretKey, VssKeyPair, VssPublicKey,
                                                   randomNumber, runSecureRandom)
import           Pos.Crypto.SecretSharing         (toVssPublicKey)
import           Pos.Crypto.Signing               (PublicKey)
import           Pos.DB.Lrc                       (getRichmenSsc)
import           Pos.DHT.Model                    (sendToNeighbors)
import           Pos.Slotting                     (getCurrentSlot, getSlotStart)
import           Pos.Ssc.Class.Workers            (SscWorkersClass (..))
import           Pos.Ssc.GodTossing.Core          (Commitment (..), SignedCommitment,
                                                   VssCertificate (..),
                                                   VssCertificatesMap,
                                                   genCommitmentAndOpening,
                                                   getCommitmentsMap, isCommitmentIdx,
                                                   isOpeningIdx, isSharesIdx,
                                                   mkSignedCommitment, mkVssCertificate)
import           Pos.Ssc.GodTossing.Functions     (hasCommitment, hasOpening, hasShares,
                                                   vssThreshold)
import           Pos.Ssc.GodTossing.GState        (getGlobalCerts, getStableCerts,
                                                   gtGetGlobalState)
import           Pos.Ssc.GodTossing.LocalData     (localOnNewSlot, sscProcessCertificate,
                                                   sscProcessCommitment,
                                                   sscProcessOpening, sscProcessShares)
import           Pos.Ssc.GodTossing.Richmen       (gtLrcConsumer)
import qualified Pos.Ssc.GodTossing.SecretStorage as SS
import           Pos.Ssc.GodTossing.Shares        (getOurShares)
import           Pos.Ssc.GodTossing.Toss          (computeParticipants)
import           Pos.Ssc.GodTossing.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types         (gsCommitments, gtcParticipateSsc,
                                                   gtcVssKeyPair)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtTag (..))
import           Pos.Types                        (EpochIndex, LocalSlotIndex,
                                                   SlotId (..), StakeholderId,
                                                   StakeholderId, Timestamp (..),
                                                   addressHash)
import           Pos.Util                         (AsBinary, asBinary, inAssertMode)
import           Pos.WorkMode                     (WorkMode)

instance SscWorkersClass SscGodTossing where
    sscWorkers = Tagged $ first pure onNewSlotSsc
    sscLrcConsumers = Tagged [gtLrcConsumer]

-- CHECK: @onNewSlotSsc
-- #checkNSendOurCert
onNewSlotSsc
    :: (WorkMode SscGodTossing m)
    => (WorkerSpec m, OutSpecs)
onNewSlotSsc = onNewSlotWorker True outs $ \slotId sendActions -> do
    richmen <- lrcActionOnEpochReason (siEpoch slotId)
        "couldn't get SSC richmen"
        getRichmenSsc
    localOnNewSlot slotId
    participationEnabled <- getNodeContext >>=
        atomically . readTVar . gtcParticipateSsc . ncSscContext
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    let enoughStake = ourId `HS.member` richmen
    when (participationEnabled && not enoughStake) $
        logDebug "Not enough stake to participate in MPC"
    when (participationEnabled && enoughStake) $ do
        checkNSendOurCert sendActions
        onNewSlotCommitment slotId sendActions
        onNewSlotOpening slotId sendActions
        onNewSlotShares slotId sendActions
  where
    outs = toOutSpecs [ oneMsgH (Proxy :: Proxy (DataMsg GtMsgContents))
                      , oneMsgH (Proxy :: Proxy (InvMsg StakeholderId GtMsgContents))
                      ]

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert :: forall m . (WorkMode SscGodTossing m) => Worker' m
checkNSendOurCert sendActions = do
    let sendCert resend = do
            if resend then
                logError "Our VSS certificate is in global state, but it has already expired, \
                         \apparently it's a bug, but we are announcing it just in case."
            else
                logInfo "Our VssCertificate hasn't been announced yet or TTL has expired, \
                         \we will announce it now."
            ourVssCertificate <- getOurVssCertificate
            let contents = MCVssCertificate ourVssCertificate
            sscProcessOurMessage contents
            let msg = DataMsg contents
            sendToNeighbors sendActions msg
            logDebug "Announced our VssCertificate."
    (_, ourId) <- getOurPkAndId
    sl@SlotId {..} <- getCurrentSlot
    certts <- getGlobalCerts sl
    let ourCertMB = HM.lookup ourId certts
    case ourCertMB of
        Just ourCert
            | vcExpiryEpoch ourCert >= siEpoch ->
                logDebug "Our VssCertificate has been already announced."
            | otherwise -> sendCert True
        Nothing -> sendCert False
  where
    getOurVssCertificate :: m VssCertificate
    getOurVssCertificate = do
        -- TODO: do this optimization
        -- localCerts <- VCD.certs <$> sscRunLocalQuery (view ldCertificates)
        getOurVssCertificateDo mempty
    getOurVssCertificateDo :: VssCertificatesMap -> m VssCertificate
    getOurVssCertificateDo certs = do
        (_, ourId) <- getOurPkAndId
        case HM.lookup ourId certs of
            Just c -> return c
            Nothing -> do
                ourSk <- ncSecretKey <$> getNodeContext
                ourVssKeyPair <- getOurVssKeyPair
                let vssKey = asBinary $ toVssPublicKey ourVssKeyPair
                    createOurCert =
                        mkVssCertificate ourSk vssKey .
                        (+) (vssMaxTTL - 1) . siEpoch -- TODO fix max ttl on random
                createOurCert <$> getCurrentSlot

getOurPkAndId
    :: WorkMode SscGodTossing m
    => m (PublicKey, StakeholderId)
getOurPkAndId = do
    ourPk <- ncPublicKey <$> getNodeContext
    return (ourPk, addressHash ourPk)

getOurVssKeyPair :: WorkMode SscGodTossing m => m VssKeyPair
getOurVssKeyPair = gtcVssKeyPair . ncSscContext <$> getNodeContext

-- Commitments-related part of new slot processing
onNewSlotCommitment
    :: (WorkMode SscGodTossing m)
    => SlotId -> Worker' m
onNewSlotCommitment slotId@SlotId {..} sendActions
    | not (isCommitmentIdx siSlot) = pass
    | otherwise = do
        ourId <- addressHash . ncPublicKey <$> getNodeContext
        shouldSendCommitment <- andM
            [ not . hasCommitment ourId <$> gtGetGlobalState
            , HM.member ourId <$> getStableCerts siEpoch]
        logDebug $ sformat ("shouldSendCommitment: "%shown) shouldSendCommitment
        when shouldSendCommitment $ do
            ourCommitment <- SS.getOurCommitment siEpoch
            let stillValidMsg = "We shouldn't generate secret, because we have already generated it"
            case ourCommitment of
                Just comm -> logDebug stillValidMsg >> sendOurCommitment comm ourId
                Nothing   -> onNewSlotCommDo ourId
  where
    onNewSlotCommDo ourId = do
        ourSk <- ncSecretKey <$> getNodeContext
        logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
        generated <- generateAndSetNewSecret ourSk slotId
        case generated of
            Nothing -> logWarning "I failed to generate secret for GodTossing"
            Just comm -> do
              logInfo (sformat ("Generated secret for "%ords%" epoch") siEpoch)
              sendOurCommitment comm ourId

    sendOurCommitment comm ourId = do
        sscProcessOurMessage (MCCommitment comm)
        sendOurData sendActions CommitmentMsg siEpoch 0 ourId

-- Openings-related part of new slot processing
onNewSlotOpening
    :: WorkMode SscGodTossing m
    => SlotId -> Worker' m
onNewSlotOpening SlotId {..} sendActions
    | not $ isOpeningIdx siSlot = pass
    | otherwise = do
        ourId <- addressHash . ncPublicKey <$> getNodeContext
        globalData <- gtGetGlobalState
        unless (hasOpening ourId globalData) $
            case globalData ^. gsCommitments . to getCommitmentsMap . at ourId of
                Nothing -> logDebug noCommMsg
                Just _  -> onNewSlotOpeningDo ourId
  where
    noCommMsg =
        "We're not sending opening, because there is no commitment from us in global state"
    onNewSlotOpeningDo ourId = do
        mbOpen <- SS.getOurOpening siEpoch
        case mbOpen of
            Just open -> do
                sscProcessOurMessage (MCOpening ourId open)
                sendOurData sendActions OpeningMsg siEpoch 2 ourId
            Nothing -> logWarning "We don't know our opening, maybe we started recently"

-- Shares-related part of new slot processing
onNewSlotShares
    :: (WorkMode SscGodTossing m)
    => SlotId -> Worker' m
onNewSlotShares SlotId {..} sendActions = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        sharesInBlockchain <- hasShares ourId <$> gtGetGlobalState
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- gtcVssKeyPair . ncSscContext <$> getNodeContext
        shares <- getOurShares ourVss
        let lShares = fmap asBinary shares
        unless (HM.null shares) $ do
            sscProcessOurMessage (MCShares ourId lShares)
            sendOurData sendActions SharesMsg siEpoch 4 ourId

sscProcessOurMessage
    :: WorkMode SscGodTossing m
    => GtMsgContents -> m ()
sscProcessOurMessage msg = runExceptT (sscProcessOurMessageDo msg) >>= logResult
  where
    sscProcessOurMessageDo (MCCommitment comm)     = sscProcessCommitment comm
    sscProcessOurMessageDo (MCOpening id open)     = sscProcessOpening id open
    sscProcessOurMessageDo (MCShares id shares)    = sscProcessShares id shares
    sscProcessOurMessageDo (MCVssCertificate cert) = sscProcessCertificate cert
    logResult (Right _) = logDebug "We have accepted our message"
    logResult (Left er) =
        logWarning $
        sformat ("We have rejected our message, reason: "%build) er

sendOurData
    :: (WorkMode SscGodTossing m)
    => SendActions m -> GtTag -> EpochIndex -> LocalSlotIndex -> StakeholderId -> m ()
sendOurData sendActions msgTag epoch slMultiplier ourId = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch slMultiplier
    logInfo $ sformat ("Announcing our "%build) msgTag
    let msg = InvMsg {imTag = msgTag, imKeys = one ourId}
    sendToNeighbors sendActions msg
    logDebug $ sformat ("Sent our " %build%" to neighbors") msgTag

-- Generate new commitment and opening and use them for the current
-- epoch. It is also saved in persistent storage.
--
-- Nothing is returned if node is not ready (usually it means that
-- node doesn't have recent enough blocks and needs to be
-- synchronized).
generateAndSetNewSecret
    :: forall m.
       (WorkMode SscGodTossing m, Bi Commitment)
    => SecretKey
    -> SlotId -- ^ Current slot
    -> m (Maybe SignedCommitment)
generateAndSetNewSecret sk SlotId {..} = do
    richmen <-
        lrcActionOnEpochReason siEpoch "couldn't get SSC richmen" getRichmenSsc
    certs <- getStableCerts siEpoch
    inAssertMode $ do
        let participantIds =
                map (addressHash . vcSigningKey) $
                computeParticipants richmen certs
        logDebug $
            sformat ("generating secret for: " %listJson) $ participantIds
    let participants =
            nonEmpty . map vcVssKey . toList $
            computeParticipants richmen certs
    maybe (Nothing <$ warnNoPs) generateAndSetNewSecretDo participants
  where
    warnNoPs =
        logWarning "generateAndSetNewSecret: can't generate, no participants"
    reportDeserFail = logError "Wrong participants list: can't deserialize"
    generateAndSetNewSecretDo :: NonEmpty (AsBinary VssPublicKey)
                              -> m (Maybe SignedCommitment)
    generateAndSetNewSecretDo ps = do
        let threshold = vssThreshold $ length ps
        mPair <- runMaybeT (genCommitmentAndOpening threshold ps)
        case mPair of
            Just (mkSignedCommitment sk siEpoch -> comm, op) ->
                Just comm <$ SS.putOurSecret comm op siEpoch
            _ -> Nothing <$ reportDeserFail

randomTimeInInterval
    :: WorkMode SscGodTossing m
    => Microsecond -> m Microsecond
randomTimeInInterval interval =
    -- Type applications here ensure that the same time units are used.
    (fromInteger @Microsecond) <$>
    liftIO (runSecureRandom (randomNumber n))
  where
    n = toInteger @Microsecond interval

waitUntilSend
    :: WorkMode SscGodTossing m
    => GtTag -> EpochIndex -> LocalSlotIndex -> m ()
waitUntilSend msgTag epoch slMultiplier = do
    Timestamp beginning <-
        getSlotStart $
        SlotId {siEpoch = epoch, siSlot = slMultiplier * slotSecurityParam}
    curTime <- currentTime
    let minToSend = curTime
    let maxToSend = beginning + mpcSendInterval
    when (minToSend < maxToSend) $ do
        let delta = maxToSend - minToSend
        timeToWait <- randomTimeInInterval delta
        let ttwMillisecond :: Millisecond
            ttwMillisecond = convertUnit timeToWait
        logDebug $
            sformat
                ("Waiting for " %shown % " before sending " %build)
                ttwMillisecond
                msgTag
        delay timeToWait
