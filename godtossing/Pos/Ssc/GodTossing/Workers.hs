{-# LANGUAGE RankNTypes #-}

-- Don't complain about deprecated ErrorT
{-# OPTIONS -Wno-deprecations #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Universum

import           Control.Concurrent.STM                (readTVar)
import           Control.Lens                          (at, each, partsOf, to, views)
import           Control.Monad.Error                   (runErrorT)
import           Control.Monad.Except                  (runExceptT)
import qualified Data.HashMap.Strict                   as HM
import qualified Data.List.NonEmpty                    as NE
import           Data.Tagged                           (Tagged)
import           Data.Time.Units                       (Microsecond, Millisecond,
                                                        convertUnit)
import           Formatting                            (build, int, ords, sformat, shown,
                                                        (%))
import           Mockable                              (currentTime, delay)
import           Serokell.Util.Exceptions              ()
import           Serokell.Util.Text                    (listJson)
import           System.Wlog                           (logDebug, logError, logInfo,
                                                        logWarning)

import           Pos.Binary.Class                      (AsBinary, Bi, asBinary)
import           Pos.Binary.GodTossing                 ()
import           Pos.Binary.Infra                      ()
import           Pos.Communication.Protocol            (EnqueueMsg, Message, MsgType (..),
                                                        Origin (..), OutSpecs,
                                                        SendActions (..), Worker,
                                                        WorkerSpec, localWorker,
                                                        onNewSlotWorker)
import           Pos.Communication.Relay               (DataMsg, ReqOrRes,
                                                        invReqDataFlowTK)
import           Pos.Communication.Specs               (createOutSpecs)
import           Pos.Communication.Types.Relay         (InvOrData, InvOrDataTK)
import           Pos.Core                              (EpochIndex, HasConfiguration,
                                                        SlotId (..), StakeholderId,
                                                        Timestamp (..),
                                                        VssCertificate (..),
                                                        VssCertificatesMap, addressHash,
                                                        bvdMpcThd, getOurSecretKey,
                                                        getOurStakeholderId, getSlotIndex,
                                                        mkLocalSlotIndex,
                                                        mkVssCertificate,
                                                        slotSecurityParam,
                                                        blkSecurityParam,
                                                        vssMaxTTL)
import           Pos.Crypto                            (SecretKey, VssKeyPair,
                                                        VssPublicKey, randomNumber,
                                                        runSecureRandom, vssKeyGen)
import           Pos.Crypto.SecretSharing              (toVssPublicKey)
import           Pos.DB                                (gsAdoptedBVData)
import           Pos.Infra.Configuration               (HasInfraConfiguration)
import           Pos.Lrc.Context                       (lrcActionOnEpochReason)
import           Pos.Lrc.Types                         (RichmenStakes)
import           Pos.Recovery.Info                     (recoveryCommGuard)
import           Pos.Reporting                         (reportMisbehaviour)
import           Pos.Slotting                          (getCurrentSlot,
                                                        getSlotStartEmpatically,
                                                        onNewSlot)
import           Pos.Ssc.Class                         (HasSscContext (..),
                                                        SscWorkersClass (..))
import           Pos.Ssc.GodTossing.Behavior           (GtBehavior (..),
                                                        GtOpeningParams (..),
                                                        GtSharesParams (..))
import           Pos.Ssc.GodTossing.Configuration      (HasGtConfiguration,
                                                        mdNoCommitmentsEpochThreshold,
                                                        mpcSendInterval)
import           Pos.Ssc.GodTossing.Core               (Commitment (..), SignedCommitment,
                                                        genCommitmentAndOpening,
                                                        getCommitmentsMap,
                                                        isCommitmentIdx, isOpeningIdx,
                                                        isSharesIdx, mkSignedCommitment)
import           Pos.Ssc.GodTossing.Functions          (hasCommitment, hasOpening,
                                                        hasShares, vssThreshold)
import           Pos.Ssc.GodTossing.GState             (getGlobalCerts, getStableCerts,
                                                        gtGetGlobalState)
import           Pos.Ssc.GodTossing.LocalData          (localOnNewSlot,
                                                        sscProcessCertificate,
                                                        sscProcessCommitment,
                                                        sscProcessOpening,
                                                        sscProcessShares)
import           Pos.Ssc.GodTossing.Network.Constraint (GtMessageConstraints)
import           Pos.Ssc.GodTossing.Richmen            (gtLrcConsumer)
import qualified Pos.Ssc.GodTossing.SecretStorage      as SS
import           Pos.Ssc.GodTossing.Shares             (getOurShares)
import           Pos.Ssc.GodTossing.Toss               (computeParticipants,
                                                        computeSharesDistrPure)
import           Pos.Ssc.GodTossing.Type               (SscGodTossing)
import           Pos.Ssc.GodTossing.Types              (gsCommitments, gtcBehavior,
                                                        gtcParticipateSsc, gtcVssKeyPair)
import           Pos.Ssc.GodTossing.Types.Message      (GtTag (..), MCCommitment (..),
                                                        MCOpening (..), MCShares (..),
                                                        MCVssCertificate (..))
import           Pos.Ssc.Mode                          (SscMode)
import           Pos.Ssc.RichmenComponent              (getRichmenSsc)
import           Pos.Util.Util                         (getKeys, inAssertMode,
                                                        leftToPanic)

instance GtMessageConstraints => SscWorkersClass SscGodTossing where
    sscWorkers = merge [onNewSlotSsc, checkForIgnoredCommitmentsWorker]
      where
        merge = mconcat . map (first pure)
    sscLrcConsumers = [gtLrcConsumer]

shouldParticipate :: (SscMode SscGodTossing ctx m) => EpochIndex -> m Bool
shouldParticipate epoch = do
    richmen <- lrcActionOnEpochReason epoch
        "couldn't get SSC richmen"
        getRichmenSsc
    participationEnabled <- view sscContext >>=
        atomically . readTVar . gtcParticipateSsc
    ourId <- getOurStakeholderId
    let enoughStake = ourId `HM.member` richmen
    when (participationEnabled && not enoughStake) $
        logDebug "Not enough stake to participate in MPC"
    return (participationEnabled && enoughStake)

-- CHECK: @onNewSlotSsc
-- #checkNSendOurCert
onNewSlotSsc
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => (WorkerSpec m, OutSpecs)
onNewSlotSsc = onNewSlotWorker True outs $ \slotId sendActions ->
    recoveryCommGuard $ do
        localOnNewSlot slotId
        whenM (shouldParticipate $ siEpoch slotId) $ do
            behavior <- view sscContext >>=
                atomically . readTVar . gtcBehavior
            checkNSendOurCert sendActions
            onNewSlotCommitment slotId sendActions
            onNewSlotOpening (gbSendOpening behavior) slotId sendActions
            onNewSlotShares (gbSendShares behavior) slotId sendActions
  where
    outs = mconcat
        [ createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCCommitment))
        , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCOpening))
        , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCShares))
        , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCVssCertificate))
        ]

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert
    :: forall ctx m.
       (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => Worker m
checkNSendOurCert sendActions = do
    ourId <- getOurStakeholderId
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
            _ <- invReqDataFlowTK "ssc" (enqueueMsg sendActions) (MsgMPC OriginSender) ourId contents
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
        ourId <- getOurStakeholderId
        case HM.lookup ourId certs of
            Just c -> return c
            Nothing -> do
                ourSk <- getOurSecretKey
                ourVssKeyPair <- getOurVssKeyPair
                let vssKey = asBinary $ toVssPublicKey ourVssKeyPair
                    createOurCert =
                        mkVssCertificate ourSk vssKey .
                        (+) (vssMaxTTL - 1) . siEpoch
                return $ createOurCert slot

getOurVssKeyPair :: SscMode SscGodTossing ctx m => m VssKeyPair
getOurVssKeyPair = views sscContext gtcVssKeyPair

-- Commitments-related part of new slot processing
onNewSlotCommitment
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => SlotId -> Worker m
onNewSlotCommitment slotId@SlotId {..} sendActions
    | not (isCommitmentIdx siSlot) = pass
    | otherwise = do
        ourId <- getOurStakeholderId
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
        ourSk <- getOurSecretKey
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
        sendOurData (enqueueMsg sendActions) CommitmentMsg ourId msg siEpoch 0

-- Openings-related part of new slot processing
onNewSlotOpening
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => GtOpeningParams -> SlotId -> Worker m
onNewSlotOpening params SlotId {..} sendActions
    | not $ isOpeningIdx siSlot = pass
    | otherwise = do
        ourId <- getOurStakeholderId
        globalData <- gtGetGlobalState
        unless (hasOpening ourId globalData) $
            case globalData ^. gsCommitments . to getCommitmentsMap . at ourId of
                Nothing -> logDebug noCommMsg
                Just _  -> SS.getOurOpening siEpoch >>= \case
                    Nothing   -> logWarning noOpenMsg
                    Just open -> sendOpening ourId open
  where
    noCommMsg =
        "We're not sending opening, because there is no commitment \
        \from us in global state"
    noOpenMsg =
        "We don't know our opening, maybe we started recently"
    sendOpening ourId open = do
        mbOpen' <- case params of
            GtOpeningNone   -> pure Nothing
            GtOpeningNormal -> pure (Just open)
            GtOpeningWrong  -> do
                keys <- NE.fromList . map (asBinary . toVssPublicKey) <$>
                        replicateM 6 vssKeyGen
                runErrorT (genCommitmentAndOpening 3 keys) >>= \case
                    Right (_, o) -> pure (Just o)
                    Left (err :: String) ->
                        logError ("onNewSlotOpening: " <> toText err)
                        $> Nothing
        whenJust mbOpen' $ \open' -> do
            let msg = MCOpening ourId open'
            sscProcessOurMessage (sscProcessOpening ourId open')
            sendOurData (enqueueMsg sendActions) OpeningMsg ourId msg siEpoch 2

-- Shares-related part of new slot processing
onNewSlotShares
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => GtSharesParams -> SlotId -> Worker m
onNewSlotShares params SlotId {..} sendActions = do
    ourId <- getOurStakeholderId
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        sharesInBlockchain <- hasShares ourId <$> gtGetGlobalState
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- views sscContext gtcVssKeyPair
        sendShares ourId =<< getOurShares ourVss
  where
    sendShares ourId shares = do
        let shares' = case params of
                GtSharesNone   -> mempty
                GtSharesNormal -> shares
                GtSharesWrong  ->
                    -- Take the list of items in the map, reverse it, put
                    -- items back. NB: this is different from “map reverse”!
                    -- We don't reverse lists of shares, we reassign those
                    -- lists to different keys.
                    shares & partsOf each %~ reverse
        unless (HM.null shares') $ do
            let lShares = fmap (map asBinary) shares'
            let msg = MCShares ourId lShares
            sscProcessOurMessage (sscProcessShares ourId lShares)
            sendOurData (enqueueMsg sendActions) SharesMsg ourId msg siEpoch 4

sscProcessOurMessage
    :: (Buildable err, SscMode SscGodTossing ctx m)
    => ExceptT err m () -> m ()
sscProcessOurMessage action =
    runExceptT action >>= logResult
  where
    logResult (Right _) = logDebug "We have accepted our message"
    logResult (Left er) =
        logWarning $
        sformat ("We have rejected our message, reason: "%build) er

sendOurData ::
    ( SscMode SscGodTossing ctx m
    , Bi (DataMsg contents)
    , Typeable contents
    , Message (InvOrData (Tagged contents StakeholderId) contents)
    , Message (ReqOrRes (Tagged contents StakeholderId))
    , HasInfraConfiguration
    , HasGtConfiguration
    )
    => EnqueueMsg m
    -> GtTag
    -> StakeholderId
    -> contents
    -> EpochIndex
    -> Word16
    -> m ()
sendOurData enqueue msgTag ourId dt epoch slMultiplier = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch slMultiplier
    logInfo $ sformat ("Announcing our "%build) msgTag
    _ <- invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) ourId dt
    logDebug $ sformat ("Sent our " %build%" to neighbors") msgTag

-- Generate new commitment and opening and use them for the current
-- epoch. It is also saved in persistent storage.
--
-- Nothing is returned if node is not ready (usually it means that
-- node doesn't have recent enough blocks and needs to be
-- synchronized).
generateAndSetNewSecret
    :: forall ctx m.
       (HasGtConfiguration, HasConfiguration, SscMode SscGodTossing ctx m, Bi Commitment)
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
    here s = "generateAndSetNewSecret: " <> s
    warnNoPs = logWarning (here "can't generate, no participants")
    generateAndSetNewSecretDo :: RichmenStakes
                              -> NonEmpty (StakeholderId, AsBinary VssPublicKey)
                              -> m (Maybe SignedCommitment)
    generateAndSetNewSecretDo richmen ps = do
        let onLeft er =
                Nothing <$
                logWarning
                (here $ sformat ("Couldn't compute shares distribution, reason: "%build) er)
        mpcThreshold <- bvdMpcThd <$> gsAdoptedBVData
        distrET <- runExceptT (computeSharesDistrPure richmen mpcThreshold)
        flip (either onLeft) distrET $ \distr -> do
            logDebug $ here $ sformat ("Computed shares distribution: "%listJson) (HM.toList distr)
            let threshold = vssThreshold $ sum $ toList distr
            let multiPSmb = nonEmpty $
                            concatMap (\(c, x) -> replicate (fromIntegral c) x) $
                            NE.map (first $ flip (HM.lookupDefault 0) distr) ps
            case multiPSmb of
                Nothing -> Nothing <$ logWarning (here "Couldn't compute participant's vss")
                Just multiPS ->
                    -- we use runErrorT and not runExceptT because we want
                    -- to get errors produced by 'fail'. In the future we'll
                    -- use MonadError everywhere and it won't be needed.
                    runErrorT (genCommitmentAndOpening threshold multiPS) >>= \case
                        Left (toText @String -> err) ->
                            logError (here err) $> Nothing
                        Right (comm, open) -> do
                            let signedComm = mkSignedCommitment sk siEpoch comm
                            SS.putOurSecret signedComm open siEpoch
                            pure (Just signedComm)

randomTimeInInterval
    :: SscMode SscGodTossing ctx m
    => Microsecond -> m Microsecond
randomTimeInInterval interval =
    -- Type applications here ensure that the same time units are used.
    (fromInteger @Microsecond) <$>
    liftIO (runSecureRandom (randomNumber n))
  where
    n = toInteger @Microsecond interval

waitUntilSend
    :: (HasInfraConfiguration, HasGtConfiguration, SscMode SscGodTossing ctx m)
    => GtTag -> EpochIndex -> Word16 -> m ()
waitUntilSend msgTag epoch slMultiplier = do
    let slot =
            leftToPanic "waitUntilSend: " $
            mkLocalSlotIndex $ slMultiplier * fromIntegral slotSecurityParam
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

----------------------------------------------------------------------------
-- Security check
----------------------------------------------------------------------------

checkForIgnoredCommitmentsWorker
    :: forall ctx m.
       (HasInfraConfiguration, HasGtConfiguration, SscMode SscGodTossing ctx m)
    => (WorkerSpec m, OutSpecs)
checkForIgnoredCommitmentsWorker = localWorker $ do
    counter <- newTVarIO 0
    void $ onNewSlot True (checkForIgnoredCommitmentsWorkerImpl counter)

-- This worker checks whether our commitments appear in blocks. This
-- check is done only if we actually should participate in
-- GodTossing. It's triggered if there are
-- 'mdNoCommitmentsEpochThreshold' consequent epochs during which we
-- had to participate in GodTossing, but our commitment didn't appear
-- in blocks. If check fails, it's reported as non-critical misbehavior.
--
-- The first argument is a counter which is incremented every time we
-- detect unexpected absence of our commitment and is reset to 0 when
-- our commitment appears in blocks.
checkForIgnoredCommitmentsWorkerImpl
    :: forall ctx m. (HasInfraConfiguration, HasGtConfiguration, SscMode SscGodTossing ctx m)
    => TVar Word -> SlotId -> m ()
checkForIgnoredCommitmentsWorkerImpl counter SlotId {..}
    -- It's enough to do this check once per epoch near the end of the epoch.
    | getSlotIndex siSlot /= 9 * fromIntegral blkSecurityParam = pass
    | otherwise =
        recoveryCommGuard $
        whenM (shouldParticipate siEpoch) $ do
            ourId <- getOurStakeholderId
            globalCommitments <-
                getCommitmentsMap . view gsCommitments <$> gtGetGlobalState
            case globalCommitments ^. at ourId of
                Nothing -> do
                    -- `modifyTVar'` returns (), hence not used
                    newCounterValue <-
                        atomically $ do
                            !x <- succ <$> readTVar counter
                            x <$ writeTVar counter x
                    when (newCounterValue > mdNoCommitmentsEpochThreshold) $ do
    -- REPORT:MISBEHAVIOUR(F) Possible eclipse attack was detected:
    -- our commitments don't get included into blockchain
                        let msg = sformat warningFormat newCounterValue
                        reportMisbehaviour False msg
                Just _ -> atomically $ writeTVar counter 0
  where
    warningFormat =
        "Our neighbors are likely trying to carry out an eclipse attack! "%
        "Out commitment didn't appear in blocks for "%int%" epochs in a row :("
