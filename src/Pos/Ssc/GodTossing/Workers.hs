{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Concurrent.STM           (readTVar)
import           Control.Lens                     (view, _2, _3)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import qualified Data.List.NonEmpty               as NE
import           Data.Tagged                      (Tagged (..))
import           Data.Time.Units                  (Microsecond, Millisecond, convertUnit)
import           Formatting                       (bprint, build, ords, sformat, shown,
                                                   (%))
import           Mockable                         (currentTime, delay)
import           Node                             (SendActions)
import           Serokell.Util.Exceptions         ()
import           Serokell.Util.Text               (listJson)
import           System.Wlog                      (logDebug, logError, logWarning, logInfo)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Binary.Relay                 ()
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.BiP            (BiP)
import           Pos.Constants                    (k, mpcSendInterval, vssMaxTTL)
import           Pos.Context                      (getNodeContext, lrcActionOnEpochReason,
                                                   ncPublicKey, ncSecretKey, ncSscContext)
import           Pos.Crypto                       (SecretKey, VssKeyPair, VssPublicKey,
                                                   randomNumber, runSecureRandom,
                                                   shortHashF, toPublic)
import           Pos.Crypto.SecretSharing         (toVssPublicKey)
import           Pos.Crypto.Signing               (PublicKey)
import           Pos.DB.GState                    (getTip)
import           Pos.DB.Lrc                       (getRichmenSsc)
import           Pos.DHT.Model                    (sendToNeighbors)
import           Pos.Slotting                     (getCurrentSlot, getSlotStart,
                                                   onNewSlot)
import           Pos.Ssc.Class.Workers            (SscWorkersClass (..))
import           Pos.Ssc.Extra.MonadLD            (sscRunLocalQuery)
import           Pos.Ssc.GodTossing.Functions     (checkCommShares, computeParticipants,
                                                   genCommitmentAndOpening, hasCommitment,
                                                   hasOpening, hasShares,
                                                   hasVssCertificate, isCommitmentIdx,
                                                   isOpeningIdx, isSharesIdx,
                                                   mkSignedCommitment, vssThreshold)
import           Pos.Ssc.GodTossing.LocalData     (getLocalPayload, localOnNewSlot,
                                                   sscProcessMessage)
import           Pos.Ssc.GodTossing.Richmen       (gtLrcConsumer)
import           Pos.Ssc.GodTossing.SecretStorage (getSecret, getSecretNEpoch, setSecret)
import           Pos.Ssc.GodTossing.Shares        (getOurShares)
import           Pos.Ssc.GodTossing.Storage       (getGlobalCerts, gtGetGlobalState)
import           Pos.Ssc.GodTossing.Types         (Commitment, Opening, SignedCommitment,
                                                   SscGodTossing, VssCertificate (..),
                                                   VssCertificatesMap, gtcParticipateSsc,
                                                   gtcVssKeyPair, mkVssCertificate,
                                                   _gpCertificates)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtMsgTag (..))
import           Pos.Types                        (EpochIndex, LocalSlotIndex,
                                                   SlotId (..), StakeholderId,
                                                   StakeholderId, Timestamp (..),
                                                   addressHash)
import           Pos.Util                         (AsBinary, asBinary, inAssertMode)
import           Pos.Util.Relay                   (DataMsg (..), InvMsg (..))
import           Pos.WorkMode                     (WorkMode)

instance SscWorkersClass SscGodTossing where
    sscWorkers = Tagged [onStart, onNewSlotSsc]
    sscLrcConsumers = Tagged [gtLrcConsumer]

-- CHECK: @onStart
-- #checkNSendOurCert
onStart :: forall m. (WorkMode SscGodTossing m) => SendActions BiP m -> m ()
onStart = checkNSendOurCert

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert :: forall m . (WorkMode SscGodTossing m) => SendActions BiP m -> m ()
checkNSendOurCert sendActions = do
    (_, ourId) <- getOurPkAndId
    sl@SlotId {..} <- getCurrentSlot
    certts <- getGlobalCerts sl
    let ourCertMB = HM.lookup ourId certts
    case ourCertMB of
        Just ourCert ->
            if vcExpiryEpoch ourCert > siEpoch then
                logDebug "Our VssCertificate has been already announced."
            else
                sendCert siEpoch True ourId
        Nothing -> sendCert siEpoch False ourId
  where
    sendCert epoch resend ourId = do
        if resend then
            logInfo "TTL will expire in the next epoch, we will announce it now."
        else
            logInfo "Our VssCertificate hasn't been announced yet, TTL has expired,\
                     \we will announce it now."
        ourVssCertificate <- getOurVssCertificate
        let contents = MCVssCertificate ourVssCertificate
        sscProcessOurMessage epoch contents ourId
        let msg = DataMsg contents ourId
    -- [CSL-245]: do not catch all, catch something more concrete.
        (sendToNeighbors sendActions msg >>
         logDebug "Announced our VssCertificate.")
        `catchAll` \e ->
            logError $ sformat ("Error announcing our VssCertificate: " % shown) e
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

-- CHECK: @onNewSlotSsc
-- #checkNSendOurCert
onNewSlotSsc
    :: (WorkMode SscGodTossing m)
    => SendActions BiP m
    -> m ()
onNewSlotSsc sendActions = onNewSlot True $ \slotId -> do
    richmen <- HS.fromList . NE.toList <$>
        lrcActionOnEpochReason (siEpoch slotId)
            "couldn't get SSC richmen"
            getRichmenSsc
    localOnNewSlot richmen slotId
    participationEnabled <- getNodeContext >>=
        atomically . readTVar . gtcParticipateSsc . ncSscContext
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    let enoughStake = ourId `HS.member` richmen
    when (participationEnabled && enoughStake) $ do
        checkNSendOurCert sendActions
        onNewSlotCommitment sendActions slotId
        onNewSlotOpening sendActions slotId
        onNewSlotShares sendActions slotId

-- Commitments-related part of new slot processing
onNewSlotCommitment
    :: (WorkMode SscGodTossing m)
    => SendActions BiP m
    -> SlotId -> m ()
onNewSlotCommitment sendActions slotId@SlotId {..}
    | not (isCommitmentIdx siSlot) = pass
    | otherwise = do
        ourId <- addressHash . ncPublicKey <$> getNodeContext
        ourSk <- ncSecretKey <$> getNodeContext
        shouldSendCommitment <- andM
            [ not . hasCommitment siEpoch ourId <$> gtGetGlobalState
            , hasVssCertificate ourId <$> gtGetGlobalState]
        logDebug $ sformat ("shouldSendCommitment: "%shown) shouldSendCommitment
        when shouldSendCommitment $ do
            richmen <-
                lrcActionOnEpochReason siEpoch "couldn't get SSC richmen" getRichmenSsc
            participants <- map vcVssKey . toList . computeParticipants richmen
                <$> getGlobalCerts slotId
            shouldCreateCommitment <- do
                se <- getSecretNEpoch
                pure . maybe True (\((_, comm, _), e) -> not $
                                     siEpoch == e &&
                                     checkCommShares participants comm) $ se
            let msg = "We shouldn't generate secret, because it is still valid"
            unless shouldCreateCommitment $ logDebug msg
            when shouldCreateCommitment $ do
                logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
                generated <- generateAndSetNewSecret ourSk slotId
                case generated of
                    Nothing -> logWarning "I failed to generate secret for GodTossing"
                    Just _ -> logInfo $
                        sformat ("Generated secret for "%ords%" epoch") siEpoch

            mbComm <- fmap (view _2) <$> getSecret
            whenJust mbComm $ \comm -> do
                sscProcessOurMessage siEpoch (MCCommitment comm) ourId
                sendOurData sendActions CommitmentMsg siEpoch 0 ourId

-- Openings-related part of new slot processing
onNewSlotOpening
    :: WorkMode SscGodTossing m
    => SendActions BiP m -> SlotId -> m ()
onNewSlotOpening sendActions SlotId {..} = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    shouldSendOpening <- do
        globalData <- gtGetGlobalState
        let openingInBlockchain = hasOpening ourId globalData
        let commitmentInBlockchain = hasCommitment siEpoch ourId globalData
        return $ and [ isOpeningIdx siSlot
                     , not openingInBlockchain
                     , commitmentInBlockchain]
    when shouldSendOpening $ do
        mbOpen <- fmap (view _3) <$> getSecret
        whenJust mbOpen $ \open -> do
            sscProcessOurMessage siEpoch (MCOpening open) ourId
            sendOurData sendActions OpeningMsg siEpoch 2 ourId

-- Shares-related part of new slot processing
onNewSlotShares
    :: (WorkMode SscGodTossing m)
    => SendActions BiP m -> SlotId -> m ()
onNewSlotShares sendActions SlotId {..} = do
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
        Just r -> do
            sscProcessMessage r msg ourId >>= logResult
  where
    logResult True  = logDebug "We have accepted our message"
    logResult False = logWarning "We have rejected our message"

sendOurData
    :: (WorkMode SscGodTossing m)
    => SendActions BiP m -> GtMsgTag -> EpochIndex -> LocalSlotIndex -> StakeholderId -> m ()
sendOurData sendActions msgTag epoch kMultiplier ourId = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch kMultiplier
    logInfo $ sformat ("Announcing our "%build) msgTag
    let msg = InvMsg {imTag = msgTag, imKeys = pure ourId}
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors sendActions msg
    logDebug $ sformat ("Sent our " %build%" to neighbors") msgTag

-- | Generate new commitment and opening and use them for the current
-- epoch. 'prepareSecretToNewSlot' must be called before doing it.
--
-- Nothing is returned if node is not ready (usually it means that
-- node doesn't have recent enough blocks and needs to be
-- synchronized).
generateAndSetNewSecret
    :: forall m.
       (WorkMode SscGodTossing m, Bi Commitment)
    => SecretKey
    -> SlotId -- ^ Current slot
    -> m (Maybe (SignedCommitment, Opening))
generateAndSetNewSecret sk sl@SlotId {..} = do
    richmen <-
        lrcActionOnEpochReason siEpoch "couldn't get SSC richmen" getRichmenSsc
    certs <- getGlobalCerts sl
    inAssertMode $ do
        let participantIds =
                map (addressHash . vcSigningKey) $
                computeParticipants richmen certs
        logDebug $
            sformat ("generating secret for: " %listJson) $
                map (bprint shortHashF) participantIds
    let participants =
            NE.nonEmpty . map vcVssKey . toList $
            computeParticipants richmen certs
    maybe (Nothing <$ warnNoPs) generateAndSetNewSecretDo participants
  where
    warnNoPs =
        logWarning "generateAndSetNewSecret: can't generate, no participants"
    reportDeserFail = logError "Wrong participants list: can't deserialize"
    generateAndSetNewSecretDo :: NE.NonEmpty (AsBinary VssPublicKey)
                              -> m (Maybe (SignedCommitment, Opening))
    generateAndSetNewSecretDo ps = do
        let threshold = vssThreshold $ length ps
        mPair <- runMaybeT (genCommitmentAndOpening threshold ps)
        tip <- getTip
        case mPair of
            Just (mkSignedCommitment sk siEpoch -> comm, op) ->
                Just (comm, op) <$ setSecret (toPublic sk, comm, op) siEpoch tip
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
waitUntilSend msgTag epoch kMultiplier = do
    Timestamp beginning <-
        getSlotStart $ SlotId {siEpoch = epoch, siSlot = kMultiplier * k}
    curTime <- currentTime
    let minToSend = curTime
    let maxToSend = beginning + mpcSendInterval
    when (minToSend < maxToSend) $ do
        let delta = maxToSend - minToSend
        timeToWait <- randomTimeInInterval delta
        let ttwMillisecond :: Millisecond
            ttwMillisecond = convertUnit timeToWait
        logDebug $
            sformat ("Waiting for "%shown%" before sending "%build)
                ttwMillisecond msgTag
        delay timeToWait
