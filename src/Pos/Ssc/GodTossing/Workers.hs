{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Concurrent.STM           (readTVar)
import           Control.Lens                     (at)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import qualified Data.List.NonEmpty               as NE
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
import           Pos.Communication.Protocol       (ConversationActions (..), NodeId,
                                                   OutSpecs, SendActions, Worker',
                                                   WorkerSpec, convH, onNewSlotWorker,
                                                   toOutSpecs)
import           Pos.Communication.Relay          (InvMsg (..), InvOrData, ReqMsg)
import           Pos.Constants                    (mpcSendInterval, slotSecurityParam,
                                                   vssMaxTTL)
import           Pos.Context                      (getNodeContext, lrcActionOnEpochReason,
                                                   ncPublicKey, ncSecretKey, ncSscContext)
import           Pos.Crypto                       (SecretKey, VssKeyPair, VssPublicKey,
                                                   randomNumber, runSecureRandom)
import           Pos.Crypto.SecretSharing         (toVssPublicKey)
import           Pos.Crypto.Signing               (PublicKey)
import           Pos.DB.Lrc                       (getRichmenSsc)
import           Pos.DHT.Model                    (converseToNeighbors)
import           Pos.Slotting                     (getCurrentSlot, getSlotStart)
import           Pos.Ssc.Class.Workers            (SscWorkersClass (..))
import           Pos.Ssc.Extra.MonadLD            (sscRunLocalQuery)
import           Pos.Ssc.GodTossing.Functions     (computeParticipants,
                                                   genCommitmentAndOpening, hasCommitment,
                                                   hasOpening, hasShares, isCommitmentIdx,
                                                   isOpeningIdx, isSharesIdx,
                                                   mkSignedCommitment, vssThreshold)
import           Pos.Ssc.GodTossing.LocalData     (getLocalPayload, localOnNewSlot,
                                                   sscProcessMessage)
import           Pos.Ssc.GodTossing.Richmen       (gtLrcConsumer)
import qualified Pos.Ssc.GodTossing.SecretStorage as SS
import           Pos.Ssc.GodTossing.Shares        (getOurShares)
import           Pos.Ssc.GodTossing.Storage       (getGlobalCerts, getStableCerts,
                                                   gtGetGlobalState)
import           Pos.Ssc.GodTossing.Types         (Commitment, SignedCommitment,
                                                   SscGodTossing, VssCertificate (..),
                                                   VssCertificatesMap, gsCommitments,
                                                   gtcParticipateSsc, gtcVssKeyPair,
                                                   mkVssCertificate, _gpCertificates)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtMsgTag (..))
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
    richmen <- HS.fromList . NE.toList <$>
        lrcActionOnEpochReason (siEpoch slotId)
            "couldn't get SSC richmen"
            getRichmenSsc
    localOnNewSlot richmen slotId
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
    outs = toOutSpecs [ convH (Proxy :: Proxy (InvOrData GtMsgTag StakeholderId GtMsgContents))
                              (Proxy :: Proxy (ReqMsg StakeholderId GtMsgTag))
                      ]

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert :: forall m . (WorkMode SscGodTossing m) => Worker' m
checkNSendOurCert sendActions = do
    let sendCert epoch resend ourId = do
            if resend then
                logError "Our VSS certificate is in global state, but it has already expired, \
                         \apparently it's a bug, but we are announcing it just in case."
            else
                logInfo "Our VssCertificate hasn't been announced yet or TTL has expired, \
                         \we will announce it now."
            ourVssCertificate <- getOurVssCertificate
            let contents = MCVssCertificate ourVssCertificate
            sscProcessOurMessage epoch contents ourId
            let msg = InvMsg VssCertificateMsg (one ourId)
            -- [CSL-245]: do not catch all, catch something more concrete.
            (converseToNeighbors sendActions (sendInv msg) >>
             logDebug "Announced our VssCertificate.")
            `catchAll` \e ->
                logError $ sformat ("Error announcing our VssCertificate: " % shown) e
    (_, ourId) <- getOurPkAndId
    sl@SlotId {..} <- getCurrentSlot
    certts <- getGlobalCerts sl
    let ourCertMB = HM.lookup ourId certts
    case ourCertMB of
        Just ourCert
            | vcExpiryEpoch ourCert >= siEpoch ->
                logDebug "Our VssCertificate has been already announced."
            | otherwise -> sendCert siEpoch True ourId
        Nothing -> sendCert siEpoch False ourId
  where
    getOurVssCertificate :: m VssCertificate
    getOurVssCertificate = do
        localCerts <- _gpCertificates . snd <$> sscRunLocalQuery getLocalPayload
        getOurVssCertificateDo localCerts
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
onNewSlotCommitment slotId@SlotId {..}
    | not (isCommitmentIdx siSlot) = const pass
    | otherwise = \sendActions -> do
        let onNewSlotCommDo ourId = do
                ourSk <- ncSecretKey <$> getNodeContext
                logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
                generated <- generateAndSetNewSecret ourSk slotId
                case generated of
                    Nothing -> logWarning "I failed to generate secret for GodTossing"
                    Just comm -> do
                      logInfo (sformat ("Generated secret for "%ords%" epoch") siEpoch)
                      sendOurCommitment comm ourId

            sendOurCommitment comm ourId = do
                sscProcessOurMessage siEpoch (MCCommitment comm) ourId
                sendOurData sendActions CommitmentMsg siEpoch 0 ourId
        ourId <- addressHash . ncPublicKey <$> getNodeContext
        shouldSendCommitment <- andM
            [ not . hasCommitment siEpoch ourId <$> gtGetGlobalState
            , HM.member ourId <$> getStableCerts siEpoch]
        logDebug $ sformat ("shouldSendCommitment: "%shown) shouldSendCommitment
        when shouldSendCommitment $ do
            ourCommitment <- SS.getOurCommitment siEpoch
            let stillValidMsg = "We shouldn't generate secret, because we have already generated it"
            case ourCommitment of
                Just comm -> logDebug stillValidMsg >> sendOurCommitment comm ourId
                Nothing   -> onNewSlotCommDo ourId
        pure ()

-- Openings-related part of new slot processing
onNewSlotOpening
    :: WorkMode SscGodTossing m
    => SlotId -> Worker' m
onNewSlotOpening SlotId {..}
    | not $ isOpeningIdx siSlot = const pass
    | otherwise = \sendActions -> do
        let noCommMsg =
                "We're not sending opening, because there is no commitment from us in global state"
            onNewSlotOpeningDo ourId = do
                mbOpen <- SS.getOurOpening siEpoch
                case mbOpen of
                    Just open -> do
                        sscProcessOurMessage siEpoch (MCOpening open) ourId
                        sendOurData sendActions OpeningMsg siEpoch 2 ourId
                    Nothing -> logWarning "We don't know our opening, maybe we started recently"
        ourId <- addressHash . ncPublicKey <$> getNodeContext
        globalData <- gtGetGlobalState
        unless (hasOpening ourId globalData) $
            case globalData ^. gsCommitments . at ourId of
                Nothing -> logDebug noCommMsg
                Just _  -> onNewSlotOpeningDo ourId

-- Shares-related part of new slot processing
onNewSlotShares
    :: (WorkMode SscGodTossing m)
    => SlotId -> Worker' m
onNewSlotShares SlotId {..} = \sendActions -> do
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
            sscProcessOurMessage siEpoch (MCShares lShares) ourId
            sendOurData sendActions SharesMsg siEpoch 4 ourId

sscProcessOurMessage
    :: WorkMode SscGodTossing m
    => EpochIndex -> GtMsgContents -> StakeholderId -> m ()
sscProcessOurMessage epoch msg ourId = do
    richmen <- getRichmenSsc epoch
    case richmen of
        Nothing ->
            logWarning
                "We are processing our SSC message and don't know richmen"
        Just r -> sscProcessMessage (epoch, r) msg ourId >>= logResult
  where
    logResult (Right _) = logDebug "We have accepted our message"
    logResult (Left er) =
        logWarning $
            sformat ("We have rejected our message, reason: "%build) er

sendOurData
    :: (WorkMode SscGodTossing m)
    => SendActions m -> GtMsgTag -> EpochIndex -> LocalSlotIndex -> StakeholderId -> m ()
sendOurData sendActions msgTag epoch slMultiplier ourId = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch slMultiplier
    logInfo $ sformat ("Announcing our "%build) msgTag
    let msg = InvMsg {imTag = msgTag, imKeys = one ourId}
    converseToNeighbors sendActions (sendInv msg)
    logDebug $ sformat ("Sent our " %build%" to neighbors") msgTag

sendInv :: InvMsg StakeholderId GtMsgTag
        -> NodeId
        -> ConversationActions
               (InvOrData GtMsgTag StakeholderId GtMsgContents)
               (ReqMsg StakeholderId GtMsgTag)
               m
        -> m ()
sendInv msg __peerId ConversationActions{..} = send $ Left msg

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
    => GtMsgTag -> EpochIndex -> LocalSlotIndex -> m ()
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
