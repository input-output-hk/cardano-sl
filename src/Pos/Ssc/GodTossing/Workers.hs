{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.List.NonEmpty               as NE
import           Data.Tagged                      (Tagged (..))
import           Data.Time.Units                  (Microsecond, Millisecond, convertUnit)
import qualified Ether
import           Formatting                       (build, ords, sformat, shown, (%))
import           Mockable                         (currentTime, delay)
import           Serokell.Util.Exceptions         ()
import           Serokell.Util.Text               (listJson)
import           System.Wlog                      (logDebug, logError, logInfo,
                                                   logWarning)
import           Universum

import           Pos.Binary.Class                 (AsBinary, Bi, asBinary)
import           Pos.Binary.Communication         ()
import           Pos.Binary.Relay                 ()
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Message        (MessagePart)
import           Pos.Communication.Protocol       (OutSpecs, SendActions, Worker',
                                                   WorkerSpec, onNewSlotWorker)
import           Pos.Communication.Relay          (DataMsg, invReqDataFlowNeighborsTK)
import           Pos.Communication.Specs          (createOutSpecs)
import           Pos.Communication.Types          (InvOrDataTK)
import           Pos.Constants                    (mpcSendInterval, slotSecurityParam,
                                                   vssMaxTTL)
import           Pos.Context                      (SscContextTag, lrcActionOnEpochReason,
                                                   npPublicKey, npSecretKey)
import           Pos.Core                         (mkLocalSlotIndex)
import           Pos.Crypto                       (SecretKey, VssKeyPair, VssPublicKey,
                                                   randomNumber, runSecureRandom)
import           Pos.Crypto.SecretSharing         (toVssPublicKey)
import           Pos.Crypto.Signing               (PublicKey)
import           Pos.Lrc.DB                       (getRichmenSsc)
import           Pos.Lrc.Types                    (RichmenStake)
import           Pos.Slotting                     (getCurrentSlot,
                                                   getSlotStartEmpatically)
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
import           Pos.Ssc.GodTossing.Toss          (computeParticipants,
                                                   computeSharesDistr)
import           Pos.Ssc.GodTossing.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types         (gsCommitments, gtcParticipateSsc,
                                                   gtcVssKeyPair)
import           Pos.Ssc.GodTossing.Types.Message (GtTag (..), MCCommitment (..),
                                                   MCOpening (..), MCShares (..),
                                                   MCVssCertificate (..))
import           Pos.Types                        (EpochIndex, SlotId (..), StakeholderId,
                                                   StakeholderId, Timestamp (..),
                                                   addressHash)
import           Pos.Util                         (getKeys, inAssertMode)
import           Pos.Util.Util                    (leftToPanic)
import           Pos.WorkMode.Class               (WorkMode)

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
    participationEnabled <- Ether.ask @SscContextTag >>=
        atomically . readTVar . gtcParticipateSsc
    ourId <- Ether.asks' (addressHash . npPublicKey)
    let enoughStake = ourId `HM.member` richmen
    when (participationEnabled && not enoughStake) $
        logDebug "Not enough stake to participate in MPC"
    when (participationEnabled && enoughStake) $ do
        checkNSendOurCert sendActions
        onNewSlotCommitment slotId sendActions
        onNewSlotOpening slotId sendActions
        onNewSlotShares slotId sendActions
  where
    outs = mconcat
              [ createOutSpecs (Proxy :: Proxy (InvOrDataTK StakeholderId MCCommitment))
              , createOutSpecs (Proxy :: Proxy (InvOrDataTK StakeholderId MCOpening))
              , createOutSpecs (Proxy :: Proxy (InvOrDataTK StakeholderId MCShares))
              , createOutSpecs (Proxy :: Proxy (InvOrDataTK StakeholderId MCVssCertificate))
              ]

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert
    :: forall m.
       (WorkMode SscGodTossing m)
    => Worker' m
checkNSendOurCert sendActions = do
    (_, ourId) <- getOurPkAndId
    let sendCert resend slot = do
            if resend then
                logError "Our VSS certificate is in global state, but it has already expired, \
                         \apparently it's a bug, but we are announcing it just in case."
                else logInfo
                         "Our VssCertificate hasn't been announced yet or TTL has expired, \
                         \we will announce it now."
            ourVssCertificate <- getOurVssCertificate slot
            let contents = MCVssCertificate ourVssCertificate
            sscProcessOurMessage (sscProcessCertificate ourVssCertificate)
            invReqDataFlowNeighborsTK "ssc" sendActions ourId contents
            logDebug "Announced our VssCertificate."

    slMaybe <- getCurrentSlot
    case slMaybe of
        Nothing -> pass
        Just sl -> do
            globalCerts <- getGlobalCerts sl
            let ourCertMB = HM.lookup ourId globalCerts
            case ourCertMB of
                Just ourCert
                    | vcExpiryEpoch ourCert >= siEpoch sl ->
                        logDebug
                            "Our VssCertificate has been already announced."
                    | otherwise -> sendCert True sl
                Nothing -> sendCert False sl
  where
    getOurVssCertificate :: SlotId -> m VssCertificate
    getOurVssCertificate slot =
        -- TODO: do this optimization
        -- localCerts <- VCD.certs <$> sscRunLocalQuery (view ldCertificates)
        getOurVssCertificateDo slot mempty
    getOurVssCertificateDo :: SlotId -> VssCertificatesMap -> m VssCertificate
    getOurVssCertificateDo slot certs = do
        (_, ourId) <- getOurPkAndId
        case HM.lookup ourId certs of
            Just c -> return c
            Nothing -> do
                ourSk <- Ether.asks' npSecretKey
                ourVssKeyPair <- getOurVssKeyPair
                let vssKey = asBinary $ toVssPublicKey ourVssKeyPair
                    createOurCert =
                        mkVssCertificate ourSk vssKey .
                        (+) (vssMaxTTL - 1) . siEpoch
                return $ createOurCert slot

getOurPkAndId
    :: WorkMode SscGodTossing m
    => m (PublicKey, StakeholderId)
getOurPkAndId = do
    ourPk <- Ether.asks' npPublicKey
    return (ourPk, addressHash ourPk)

getOurVssKeyPair :: WorkMode SscGodTossing m => m VssKeyPair
getOurVssKeyPair = Ether.asks @SscContextTag gtcVssKeyPair

-- Commitments-related part of new slot processing
onNewSlotCommitment
    :: (WorkMode SscGodTossing m)
    => SlotId -> Worker' m
onNewSlotCommitment slotId@SlotId {..} sendActions
    | not (isCommitmentIdx siSlot) = pass
    | otherwise = do
        ourId <- Ether.asks' (addressHash . npPublicKey)
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
        ourSk <- Ether.asks' npSecretKey
        logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
        generated <- generateAndSetNewSecret ourSk slotId
        case generated of
            Nothing -> logWarning "I failed to generate secret for GodTossing"
            Just comm -> do
              logInfo (sformat ("Generated secret for "%ords%" epoch") siEpoch)
              sendOurCommitment comm ourId

    sendOurCommitment comm ourId = do
        let msg = MCCommitment comm
        sscProcessOurMessage (sscProcessCommitment comm)
        sendOurData sendActions CommitmentMsg ourId msg siEpoch 0

-- Openings-related part of new slot processing
onNewSlotOpening
    :: WorkMode SscGodTossing m
    => SlotId -> Worker' m
onNewSlotOpening SlotId {..} sendActions
    | not $ isOpeningIdx siSlot = pass
    | otherwise = do
        ourId <- Ether.asks' (addressHash . npPublicKey)
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
                let msg = MCOpening ourId open
                sscProcessOurMessage (sscProcessOpening ourId open)
                sendOurData sendActions OpeningMsg ourId msg siEpoch 2
            Nothing -> logWarning "We don't know our opening, maybe we started recently"

-- Shares-related part of new slot processing
onNewSlotShares
    :: (WorkMode SscGodTossing m)
    => SlotId -> Worker' m
onNewSlotShares SlotId {..} sendActions = do
    ourId <- Ether.asks' (addressHash . npPublicKey)
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        sharesInBlockchain <- hasShares ourId <$> gtGetGlobalState
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- Ether.asks @SscContextTag gtcVssKeyPair
        shares <- getOurShares ourVss
        let lShares = fmap (NE.map asBinary) shares
        unless (HM.null shares) $ do
            let msg = MCShares ourId lShares
            sscProcessOurMessage (sscProcessShares ourId lShares)
            sendOurData sendActions SharesMsg ourId msg siEpoch 4

sscProcessOurMessage
    :: (Buildable err, WorkMode SscGodTossing m)
    => ExceptT err m () -> m ()
sscProcessOurMessage action =
    runExceptT action >>= logResult
  where
    logResult (Right _) = logDebug "We have accepted our message"
    logResult (Left er) =
        logWarning $
        sformat ("We have rejected our message, reason: "%build) er

sendOurData ::
    ( WorkMode SscGodTossing m
    , MessagePart contents
    , Bi (DataMsg contents)
    , Typeable contents
    )
    => SendActions m
    -> GtTag
    -> StakeholderId
    -> contents
    -> EpochIndex
    -> Word16
    -> m ()
sendOurData sendActions msgTag ourId dt epoch slMultiplier = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch slMultiplier
    logInfo $ sformat ("Announcing our "%build) msgTag
    invReqDataFlowNeighborsTK "ssc" sendActions ourId dt
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
                computeParticipants (getKeys richmen) certs
        logDebug $
            sformat ("generating secret for: " %listJson) $ participantIds
    let participants = nonEmpty $
                       map (second vcVssKey) $
                       HM.toList $
                       computeParticipants (getKeys richmen) certs
    maybe (Nothing <$ warnNoPs) (generateAndSetNewSecretDo richmen) participants
  where
    warnNoPs =
        logWarning "generateAndSetNewSecret: can't generate, no participants"
    reportDeserFail = logError "Wrong participants list: can't deserialize"
    generateAndSetNewSecretDo :: RichmenStake
                              -> NonEmpty (StakeholderId, AsBinary VssPublicKey)
                              -> m (Maybe SignedCommitment)
    generateAndSetNewSecretDo richmen ps = do
        let onLeft er =
                Nothing <$
                logWarning
                (sformat ("Couldn't compute shares distribution, reason: "%build) er)
        distrET <- runExceptT (computeSharesDistr richmen)
        flip (either onLeft) distrET $ \distr -> do
            logDebug $ sformat ("Computed shares distribution: "%listJson) (HM.toList distr)
            let threshold = vssThreshold $ sum $ toList distr
            let multiPSmb = nonEmpty $
                            concatMap (\(c, x) -> replicate (fromIntegral c) x) $
                            NE.map (first $ flip (HM.lookupDefault 0) distr) ps
            case multiPSmb of
                Nothing -> Nothing <$ logWarning "Couldn't compute participant's vss"
                Just multiPS -> do
                    mPair <- runMaybeT (genCommitmentAndOpening threshold multiPS)
                    flip (maybe (reportDeserFail $> Nothing)) mPair $
                        \(mkSignedCommitment sk siEpoch -> comm, open) ->
                            Just comm <$ SS.putOurSecret comm open siEpoch

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
    => GtTag -> EpochIndex -> Word16 -> m ()
waitUntilSend msgTag epoch slMultiplier = do
    let slot =
            leftToPanic "waitUntilSend: " $
            mkLocalSlotIndex $ slMultiplier * slotSecurityParam
    Timestamp beginning <-
        getSlotStartEmpatically $
        SlotId {siEpoch = epoch, siSlot = slot}
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
