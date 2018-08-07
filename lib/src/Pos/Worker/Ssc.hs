{-# LANGUAGE RankNTypes #-}

module Pos.Worker.Ssc
       ( sscWorkers
       ) where

import           Universum hiding (keys)

import           Control.Concurrent.STM (readTVar)
import           Control.Lens (at, each, partsOf, to, views)
import           Control.Monad.Except (runExceptT)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Microsecond, Millisecond, convertUnit)
import           Formatting (build, ords, sformat, shown, (%))
import           Serokell.Util.Exceptions ()
import           Serokell.Util.Text (listJson)
import qualified System.Metrics.Gauge as Metrics
import           UnliftIO (MonadUnliftIO)

import qualified Crypto.Random as Rand

import           Pos.Binary.Class (AsBinary, asBinary, fromBinary)
import           Pos.Chain.Lrc (RichmenStakes)
import           Pos.Chain.Security (SecurityParams)
import           Pos.Chain.Ssc (HasSscConfiguration, HasSscContext (..),
                     MonadSscMem, SscBehavior (..), SscOpeningParams (..),
                     SscSharesParams (..), SscTag (..), computeParticipants,
                     computeSharesDistrPure, getOurShares, hasCommitment,
                     hasOpening, hasShares, isCommitmentIdx, isOpeningIdx,
                     isSharesIdx, mkSignedCommitment, mpcSendInterval,
                     scBehavior, scParticipateSsc, scVssKeyPair,
                     sgsCommitments, vssThreshold)
import           Pos.Core (EpochIndex, HasPrimaryKey, SlotId (..),
                     StakeholderId, Timestamp (..), blkSecurityParam,
                     getOurSecretKey, getOurStakeholderId, getSlotIndex,
                     mkLocalSlotIndex, slotSecurityParam, vssMaxTTL)
import           Pos.Core.Conc (currentTime, delay)
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..),
                     MisbehaviorMetrics (..), MonadReporting)
import           Pos.Core.Ssc (InnerSharesMap, Opening, SignedCommitment,
                     VssCertificate (..), VssCertificatesMap (..),
                     getCommitmentsMap, lookupVss, memberVss, mkVssCertificate,
                     randCommitmentAndOpening)
import           Pos.Core.Update (bvdMpcThd)
import           Pos.Crypto (ProtocolMagic, SecretKey, VssKeyPair, VssPublicKey,
                     randomNumber, randomNumberInRange, runSecureRandom,
                     vssKeyGen)
import           Pos.Crypto.SecretSharing (toVssPublicKey)
import           Pos.DB (gsAdoptedBVData)
import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.DB.Lrc (HasLrcContext, getSscRichmen)
import           Pos.DB.Ssc (getGlobalCerts, getStableCerts,
                     sscGarbageCollectLocalData, sscGetGlobalState,
                     sscProcessCertificate, sscProcessCommitment,
                     sscProcessOpening, sscProcessShares)
import qualified Pos.DB.Ssc.SecretStorage as SS
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Infra.Slotting (MonadSlots, defaultOnNewSlotParams,
                     getCurrentSlot, getSlotStartEmpatically, onNewSlot)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebugS,
                     logErrorS, logInfoS, logWarningS)
import           Pos.Util.Util (HasLens (..), getKeys, leftToPanic)


type SscMode ctx m
    = ( MonadIO m
      , MonadUnliftIO m
      , Rand.MonadRandom m
      , MonadMask m
      , MonadSlots ctx m
      , MonadGState m
      , MonadDB m
      , MonadSscMem ctx m
      , MonadRecoveryInfo ctx m
      , HasShutdownContext ctx
      , MonadReader ctx m
      , HasSscContext ctx
      , MonadReporting m
      , HasPrimaryKey ctx
      , HasLens SecurityParams ctx SecurityParams
      , HasLrcContext ctx
      , HasSscConfiguration
      )

sscWorkers
  :: ( SscMode ctx m
     , HasMisbehaviorMetrics ctx
     )
  => TraceNamed m -> ProtocolMagic -> [Diffusion m -> m ()]
sscWorkers logTrace pm = [ onNewSlotSsc logTrace pm
                        , checkForIgnoredCommitmentsWorker logTrace
                        ]

shouldParticipate :: SscMode ctx m => TraceNamed m -> EpochIndex -> m Bool
shouldParticipate logTrace epoch = do
    richmen <- getSscRichmen "shouldParticipate" epoch
    participationEnabled <- view sscContext >>=
        (readTVarIO . scParticipateSsc)
    ourId <- getOurStakeholderId
    let enoughStake = ourId `HM.member` richmen
    when (participationEnabled && not enoughStake) $
        logDebugS logTrace "Not enough stake to participate in MPC"
    return (participationEnabled && enoughStake)

-- CHECK: @onNewSlotSsc
-- #checkNSendOurCert
onNewSlotSsc
    :: ( SscMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> Diffusion m
    -> m ()
onNewSlotSsc logTrace0 pm = \diffusion -> onNewSlot logTrace defaultOnNewSlotParams $ \slotId ->
    recoveryCommGuard logTrace "onNewSlot worker in SSC" $ do
        sscGarbageCollectLocalData slotId
        whenM (shouldParticipate logTrace $ siEpoch slotId) $ do
            behavior <- view sscContext >>=
                (readTVarIO . scBehavior)
            checkNSendOurCert logTrace pm (sendSscCert diffusion)
            onNewSlotCommitment logTrace pm slotId (sendSscCommitment diffusion)
            onNewSlotOpening logTrace pm (sbSendOpening behavior) slotId (sendSscOpening diffusion)
            onNewSlotShares logTrace pm (sbSendShares behavior) slotId (sendSscShares diffusion)
    where
    logTrace = appendName "newslot" logTrace0

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert
    :: forall ctx m.
       ( SscMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> (VssCertificate -> m ())
    -> m ()
checkNSendOurCert logTrace pm sendCert = do
    ourId <- getOurStakeholderId
    let sendCertDo resend slot = do
            if resend then
                logErrorS logTrace "Our VSS certificate is in global state, but it has already expired, \
                         \apparently it's a bug, but we are announcing it just in case."
                else logInfoS logTrace
                         "Our VssCertificate hasn't been announced yet or TTL has expired, \
                         \we will announce it now."
            ourVssCertificate <- getOurVssCertificate slot
            sscProcessOurMessage logTrace (sscProcessCertificate logTrace pm ourVssCertificate)
            _ <- sendCert ourVssCertificate
            logDebugS logTrace "Announced our VssCertificate."

    slMaybe <- getCurrentSlot
    case slMaybe of
        Nothing -> pass
        Just sl -> do
            globalCerts <- getGlobalCerts sl
            let ourCertMB = lookupVss ourId globalCerts
            case ourCertMB of
                Just ourCert
                    | vcExpiryEpoch ourCert >= siEpoch sl ->
                        logDebugS logTrace
                            "Our VssCertificate has been already announced."
                    | otherwise -> sendCertDo True sl
                Nothing -> sendCertDo False sl
  where
    getOurVssCertificate :: SlotId -> m VssCertificate
    getOurVssCertificate slot =
        -- TODO: do this optimization
        -- localCerts <- VCD.certs <$> sscRunLocalQuery (view ldCertificates)
        getOurVssCertificateDo slot mempty
    getOurVssCertificateDo :: SlotId -> VssCertificatesMap -> m VssCertificate
    getOurVssCertificateDo slot certs = do
        ourId <- getOurStakeholderId
        case lookupVss ourId certs of
            Just c -> return c
            Nothing -> do
                ourSk <- getOurSecretKey
                ourVssKeyPair <- getOurVssKeyPair
                let vssKey = asBinary $ toVssPublicKey ourVssKeyPair
                    createOurCert =
                        mkVssCertificate pm ourSk vssKey .
                        (+) (vssMaxTTL - 1) . siEpoch
                return $ createOurCert slot

getOurVssKeyPair :: SscMode ctx m => m VssKeyPair
getOurVssKeyPair = views sscContext scVssKeyPair

-- Commitments-related part of new slot processing
onNewSlotCommitment
    :: ( SscMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> SlotId
    -> (SignedCommitment -> m ())
    -> m ()
onNewSlotCommitment logTrace pm slotId@SlotId {..} sendCommitment
    | not (isCommitmentIdx siSlot) = pass
    | otherwise = do
        ourId <- getOurStakeholderId
        shouldSendCommitment <- andM
            [ not . hasCommitment ourId <$> sscGetGlobalState
            , memberVss ourId <$> getStableCerts siEpoch]
        if shouldSendCommitment then
            logDebugS logTrace "We should send commitment"
        else
            logDebugS logTrace "We shouldn't send commitment"
        when shouldSendCommitment $ do
            ourCommitment <- SS.getOurCommitment siEpoch
            let stillValidMsg = "We shouldn't generate secret, because we have already generated it"
            case ourCommitment of
                Just comm -> logDebugS logTrace     stillValidMsg >> sendOurCommitment comm
                Nothing   -> onNewSlotCommDo
  where
    onNewSlotCommDo = do
        ourSk <- getOurSecretKey
        logDebugS logTrace $ sformat ("Generating secret for "%ords%" epoch") siEpoch
        generated <- generateAndSetNewSecret logTrace pm ourSk slotId
        case generated of
            Nothing -> logWarningS logTrace "I failed to generate secret for SSC"
            Just comm -> do
              logInfoS logTrace (sformat ("Generated secret for "%ords%" epoch") siEpoch)
              sendOurCommitment comm

    sendOurCommitment comm = do
        sscProcessOurMessage logTrace (sscProcessCommitment logTrace pm comm)
        sendOurData logTrace sendCommitment CommitmentMsg comm siEpoch 0

-- | Generate a random Opening.
randomOpening :: IO Opening
randomOpening = snd <$> secureRandCommitmentAndOpening
  where
    secureRandCommitmentAndOpening = runSecureRandom $ do
        t       <- randomNumberInRange 3 10
        n       <- randomNumberInRange (t*2-1) (t*2) -- This seems strange.
        vssKeys <- replicateM (fromInteger n) $ toVssPublicKey <$> vssKeyGen
        randCommitmentAndOpening (fromIntegral t) (NE.fromList vssKeys)

-- Openings-related part of new slot processing
onNewSlotOpening
    :: ( SscMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> SscOpeningParams     -- ^ This parameter is part of the node's
                            -- BehaviorConfig which defines how the node should
                            -- behave when it's sending openings to other nodes
    -> SlotId
    -> (Opening -> m ())
    -> m ()
onNewSlotOpening logTrace pm params SlotId {..} sendOpening
    | not $ isOpeningIdx siSlot = pass
    | otherwise = do
        ourId <- getOurStakeholderId
        globalData <- sscGetGlobalState
        unless (hasOpening ourId globalData) $
            case globalData ^. sgsCommitments . to getCommitmentsMap . at ourId of
                Nothing -> logDebugS logTrace noCommMsg
                Just _  -> SS.getOurOpening siEpoch >>= \case
                    Nothing   -> logWarningS logTrace noOpenMsg
                    Just open -> sendOpeningDo ourId open
  where
    noCommMsg =
        "We're not sending opening, because there is no commitment \
        \from us in global state"
    noOpenMsg =
        "We don't know our opening, maybe we started recently"
    sendOpeningDo ourId open = do
        mbOpen' <- case params of
            -- The node doesn't send an 'Opening'.
            SscOpeningNone   -> pure Nothing
            -- The node acts as normal and sends an 'Opening'.
            SscOpeningNormal -> pure (Just open)
            -- The node sends rubbish (a random 'Opening'). This setting is
            -- typically only specified for testing purposes.
            SscOpeningWrong  -> Just <$> liftIO randomOpening
        whenJust mbOpen' $ \open' -> do
            sscProcessOurMessage logTrace (sscProcessOpening logTrace pm ourId open')
            sendOurData logTrace sendOpening OpeningMsg open' siEpoch 2

-- Shares-related part of new slot processing
onNewSlotShares
    :: ( SscMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> SscSharesParams
    -> SlotId
    -> (InnerSharesMap -> m ())
    -> m ()
onNewSlotShares logTrace pm params SlotId {..} sendShares = do
    ourId <- getOurStakeholderId
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        sharesInBlockchain <- hasShares ourId <$> sscGetGlobalState
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- views sscContext scVssKeyPair
        sendSharesDo ourId =<< getOurShares logTrace ourVss
  where
    sendSharesDo ourId shares = do
        let shares' = case params of
                SscSharesNone   -> mempty
                SscSharesNormal -> shares
                SscSharesWrong  ->
                    -- Take the list of items in the map, reverse it, put
                    -- items back. NB: this is different from “map reverse”!
                    -- We don't reverse lists of shares, we reassign those
                    -- lists to different keys.
                    shares & partsOf each %~ reverse
        unless (HM.null shares') $ do
            let lShares = fmap (map asBinary) shares'
            sscProcessOurMessage logTrace (sscProcessShares logTrace pm ourId lShares)
            sendOurData logTrace sendShares SharesMsg lShares siEpoch 4

sscProcessOurMessage
    :: (Buildable err, SscMode ctx m)
    => TraceNamed m -> m (Either err ()) -> m ()
sscProcessOurMessage logTrace action =
    action >>= logResult
  where
    logResult (Right _) = logDebugS logTrace "We have accepted our message"
    logResult (Left er) =
        logWarningS logTrace $
        sformat ("We have rejected our message, reason: "%build) er

sendOurData
    :: SscMode ctx m
    => TraceNamed m
    -> (contents -> m ())
    -> SscTag
    -> contents
    -> EpochIndex
    -> Word16
    -> m ()
sendOurData logTrace sendIt msgTag dt epoch slMultiplier = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend logTrace msgTag epoch slMultiplier
    logInfoS logTrace $ sformat ("Announcing our "%build) msgTag
    _ <- sendIt dt
    logDebugS logTrace $ sformat ("Sent our " %build%" to neighbors") msgTag

-- Generate new commitment and opening and use them for the current
-- epoch. It is also saved in persistent storage.
--
-- Nothing is returned if node is not ready (usually it means that
-- node doesn't have recent enough blocks and needs to be
-- synchronized).
generateAndSetNewSecret
    :: forall ctx m.
       ( SscMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> SecretKey
    -> SlotId -- ^ Current slot
    -> m (Maybe SignedCommitment)
generateAndSetNewSecret logTrace pm sk SlotId {..} = do
    richmen <- getSscRichmen "generateAndSetNewSecret" siEpoch
    certs <- getStableCerts siEpoch
    inAssertMode $ do
        let participantIds =
                HM.keys . getVssCertificatesMap $
                computeParticipants (getKeys richmen) certs
        logDebugS logTrace $
            sformat ("generating secret for: " %listJson) $ participantIds
    let participants = nonEmpty $
                       map (second vcVssKey) $
                       HM.toList . getVssCertificatesMap $
                       computeParticipants (getKeys richmen) certs
    maybe (Nothing <$ warnNoPs) (generateAndSetNewSecretDo richmen) participants
  where
    here s = "generateAndSetNewSecret: " <> s
    warnNoPs = logWarningS logTrace (here "can't generate, no participants")
    generateAndSetNewSecretDo :: RichmenStakes
                              -> NonEmpty (StakeholderId, AsBinary VssPublicKey)
                              -> m (Maybe SignedCommitment)
    generateAndSetNewSecretDo richmen ps = do
        let onLeft er =
                Nothing <$
                logWarningS logTrace
                (here $ sformat ("Couldn't compute shares distribution, reason: "%build) er)
        mpcThreshold <- bvdMpcThd <$> gsAdoptedBVData
        distrET <- runExceptT (computeSharesDistrPure richmen mpcThreshold)
        flip (either onLeft) distrET $ \distr -> do
            logDebugS logTrace $ here $ sformat ("Computed shares distribution: "%listJson) (HM.toList distr)
            let threshold = vssThreshold $ sum $ toList distr
            let multiPSmb = nonEmpty $
                            concatMap (\(c, x) -> replicate (fromIntegral c) x) $
                            NE.map (first $ flip (HM.lookupDefault 0) distr) ps
            case multiPSmb of
                Nothing -> Nothing <$
                    logWarningS logTrace (here "Couldn't compute participant's vss")
                Just multiPS -> case mapM fromBinary multiPS of
                    Left err -> Nothing <$
                        logErrorS logTrace (here ("Couldn't deserialize keys: " <> err))
                    Right keys -> do
                        (comm, open) <- liftIO $ runSecureRandom $
                            randCommitmentAndOpening threshold keys
                        let signedComm = mkSignedCommitment pm sk siEpoch comm
                        SS.putOurSecret signedComm open siEpoch
                        pure (Just signedComm)

randomTimeInInterval
    :: SscMode ctx m
    => Microsecond -> m Microsecond
randomTimeInInterval interval =
    -- Type applications here ensure that the same time units are used.
    (fromInteger @Microsecond) <$>
    liftIO (runSecureRandom (randomNumber n))
  where
    n = toInteger @Microsecond interval

waitUntilSend
    :: SscMode ctx m
    => TraceNamed m -> SscTag -> EpochIndex -> Word16 -> m ()
waitUntilSend logTrace msgTag epoch slMultiplier = do
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
        logDebugS logTrace $
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
       ( SscMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => TraceNamed m
    -> Diffusion m
    -> m ()
checkForIgnoredCommitmentsWorker logTrace = \_ -> do
    counter <- newTVarIO 0
    onNewSlot logTrace defaultOnNewSlotParams (checkForIgnoredCommitmentsWorkerImpl logTrace counter)

-- This worker checks whether our commitments appear in blocks. This check
-- is done only if we actually should participate in SSC. It's triggered if
-- there are 'mdNoCommitmentsEpochThreshold' consequent epochs during which
-- we had to participate in SSC, but our commitment didn't appear in blocks.
-- If check fails, it's reported as non-critical misbehavior.
--
-- The first argument is a counter which is incremented every time we
-- detect unexpected absence of our commitment and is reset to 0 when
-- our commitment appears in blocks.
checkForIgnoredCommitmentsWorkerImpl
    :: forall ctx m.
       ( SscMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => TraceNamed m -> TVar Word -> SlotId -> m ()
checkForIgnoredCommitmentsWorkerImpl logTrace counter SlotId {..}
    -- It's enough to do this check once per epoch near the end of the epoch.
    | getSlotIndex siSlot /= 9 * fromIntegral blkSecurityParam = pass
    | otherwise =
        recoveryCommGuard logTrace "checkForIgnoredCommitmentsWorker" $
        whenM (shouldParticipate logTrace siEpoch) $ do
            ourId <- getOurStakeholderId
            globalCommitments <-
                getCommitmentsMap . view sgsCommitments <$> sscGetGlobalState
            case globalCommitments ^. at ourId of
                Nothing -> do
                    -- `modifyTVar'` returns (), hence not used
                    newCounterValue <-
                        atomically $ do
                            !x <- succ <$> readTVar counter
                            x <$ writeTVar counter x
                    whenJustM (view misbehaviorMetrics) $ liftIO .
                        flip Metrics.set (fromIntegral newCounterValue) . _mmIgnoredCommitments
                Just _ -> atomically $ writeTVar counter 0
