{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Concurrent.STM           (readTVar)
import           Control.Lens                     (use, view, (%=), _2, _3)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Control.TimeWarp.Timed           (Microsecond, Millisecond, currentTime,
                                                   for, wait)
import           Data.HashMap.Strict              (insert, lookup, member)
import           Data.List.NonEmpty               (nonEmpty)
import           Data.Tagged                      (Tagged (..))
import           Data.Time.Units                  (convertUnit)
import           Formatting                       (build, ords, sformat, shown, (%))
import           Serokell.Util.Exceptions         ()
import           System.Wlog                      (logDebug, logError, logWarning)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Binary.Relay                 ()
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Methods        (sendToNeighborsSafe)
import           Pos.Constants                    (k, mpcSendInterval, vssMaxTTL)
import           Pos.Context                      (getNodeContext, lrcActionOnEpochReason,
                                                   ncPublicKey, ncSecretKey, ncSscContext)
import           Pos.Crypto                       (SecretKey, VssKeyPair, randomNumber,
                                                   runSecureRandom, shortHashF, toPublic)
import           Pos.Crypto.SecretSharing         (toVssPublicKey)
import           Pos.Crypto.Signing               (PublicKey)
import           Pos.DB.GState                    (getTip)
import           Pos.DB.Lrc                       (getRichmenSsc)
import           Pos.Slotting                     (getCurrentSlot, getSlotStart,
                                                   onNewSlot)
import           Pos.Ssc.Class.Workers            (SscWorkersClass (..))
import           Pos.Ssc.Extra.MonadLD            (sscRunLocalQuery, sscRunLocalUpdate)
import           Pos.Ssc.GodTossing.Functions     (genCommitmentAndOpening, hasCommitment,
                                                   hasOpening, hasShares, isCommitmentIdx,
                                                   isOpeningIdx, isSharesIdx,
                                                   mkSignedCommitment, vssThreshold)
import           Pos.Ssc.GodTossing.LocalData     (ldCertificates, ldLastProcessedSlot,
                                                   localOnNewSlot, sscProcessMessage)
import           Pos.Ssc.GodTossing.Richmen       (gtLrcConsumer)
import           Pos.Ssc.GodTossing.SecretStorage (getSecret, getSecretForTip, setSecret)
import           Pos.Ssc.GodTossing.Shares        (getOurShares)
import           Pos.Ssc.GodTossing.Storage       (getGlobalCerts, gtGetGlobalState)
import           Pos.Ssc.GodTossing.Types         (Commitment, Opening, SignedCommitment,
                                                   SscGodTossing, VssCertificate (..),
                                                   gtcParticipateSsc, gtcVssKeyPair,
                                                   mkVssCertificate)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtMsgTag (..))
import           Pos.Types                        (EpochIndex, LocalSlotIndex,
                                                   SlotId (..), StakeholderId,
                                                   StakeholderId, Timestamp (..),
                                                   addressHash)
import           Pos.Util                         (asBinary)
import           Pos.Util.Relay                   (DataMsg (..), InvMsg (..))
import           Pos.WorkMode                     (WorkMode)

instance SscWorkersClass SscGodTossing where
    sscWorkers = Tagged [onStart, onNewSlotSsc]
    sscLrcConsumers = Tagged [gtLrcConsumer]

-- CHECK: @onStart
-- #checkNSendOurCert
onStart :: forall m. (WorkMode SscGodTossing m) => m ()
onStart = checkNSendOurCert

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert :: forall m . (WorkMode SscGodTossing m) => m ()
checkNSendOurCert = do
    (_, ourId) <- getOurPkAndId
    isCertInBlockhain <- member ourId <$> getGlobalCerts
    if isCertInBlockhain then
       logDebug "Our VssCertificate has been already announced."
    else do
        logDebug "Our VssCertificate hasn't been announced yet or TTL has expired\
                 \, we will announce it now."
        ourVssCertificate <- getOurVssCertificate
        let msg = DataMsg (MCVssCertificate ourVssCertificate) ourId
        -- [CSL-245]: do not catch all, catch something more concrete.
        (sendToNeighborsSafe msg >> logDebug "Announced our VssCertificate.")
            `catchAll` \e ->
            logError $ sformat ("Error announcing our VssCertificate: " % shown) e
  where
    getOurVssCertificate :: m VssCertificate
    getOurVssCertificate = do
        (_, ourId) <- getOurPkAndId
        localCerts     <- sscRunLocalQuery $ view ldCertificates
        case lookup ourId localCerts of
          Just c  -> return c
          Nothing -> do
            ourSk         <- ncSecretKey <$> getNodeContext
            ourVssKeyPair <- getOurVssKeyPair
            let vssKey  = asBinary $ toVssPublicKey ourVssKeyPair
                createOurCert = mkVssCertificate ourSk vssKey .
                                (+) (vssMaxTTL - 1) . siEpoch
            sscRunLocalUpdate $ do
                lps <- use ldLastProcessedSlot
                let ourCert = createOurCert lps
                ldCertificates %= insert ourId ourCert
                return ourCert

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
    => m ()
onNewSlotSsc = onNewSlot True $ \slotId-> do
    localOnNewSlot slotId
    checkNSendOurCert
    participationEnabled <- getNodeContext >>=
        atomically . readTVar . gtcParticipateSsc . ncSscContext
    when participationEnabled $ do
        onNewSlotCommitment slotId
        onNewSlotOpening slotId
        onNewSlotShares slotId

-- Commitments-related part of new slot processing
onNewSlotCommitment
    :: (WorkMode SscGodTossing m)
    => SlotId -> m ()
onNewSlotCommitment slotId@SlotId{..} = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    ourSk <- ncSecretKey <$> getNodeContext
    tip <- getTip
    shouldSendCommitment <- do
        commitmentInBlockchain <- hasCommitment ourId <$> gtGetGlobalState
        return $ isCommitmentIdx siSlot && not commitmentInBlockchain
    when shouldSendCommitment $ do
        shouldCreateCommitment <- do
            secret <- getSecret
            secretForTip <- getSecretForTip
            if | not (isCommitmentIdx siSlot) ->
                 False <$
                 logDebug "We shouldn't generate secret, because current slot is not appropriate for it"
               | isJust secret && tip == secretForTip ->
                 False <$
                 logDebug
                 (sformat ("We shouldn't generate secret, because we have secret for current tip ("%shortHashF%")") tip)
               | otherwise -> pure True
        when shouldCreateCommitment $ do
            logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
            generated <- generateAndSetNewSecret ourSk slotId
            case generated of
                Nothing -> logWarning "I failed to generate secret for GodTossing"
                Just _ -> logDebug $
                    sformat ("Generated secret for "%ords%" epoch") siEpoch

        mbComm <- fmap (view _2) <$> getSecret
        whenJust mbComm $ \comm -> do
            _ <- sscProcessMessageRichmen (MCCommitment comm) ourId
            sendOurData CommitmentMsg siEpoch 0 ourId

-- Openings-related part of new slot processing
onNewSlotOpening
    :: (WorkMode SscGodTossing m)
    => SlotId -> m ()
onNewSlotOpening SlotId {..} = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    shouldSendOpening <- do
        globalData <- gtGetGlobalState
        let openingInBlockchain = hasOpening ourId globalData
        let commitmentInBlockchain = hasCommitment ourId globalData
        return $ and [ isOpeningIdx siSlot
                     , not openingInBlockchain
                     , commitmentInBlockchain]
    when shouldSendOpening $ do
        mbOpen <- fmap (view _3) <$> getSecret
        whenJust mbOpen $ \open -> do
            _ <- sscProcessMessageRichmen (MCOpening open) ourId
            sendOurData OpeningMsg siEpoch 2 ourId

-- Shares-related part of new slot processing
onNewSlotShares
    :: (WorkMode SscGodTossing m)
    => SlotId -> m ()
onNewSlotShares SlotId {..} = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        -- [CSL-203]: here we assume that all shares are always sent
        -- as a whole package.
        sharesInBlockchain <- hasShares ourId <$> gtGetGlobalState
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- gtcVssKeyPair . ncSscContext <$> getNodeContext
        shares <- getOurShares ourVss
        let lShares = fmap asBinary shares
        unless (null shares) $ do
            _ <- sscProcessMessageRichmen (MCShares lShares) ourId
            sendOurData SharesMsg siEpoch 4 ourId

sscProcessMessageRichmen :: WorkMode SscGodTossing m
                         => GtMsgContents
                         -> StakeholderId
                         -> m Bool
sscProcessMessageRichmen msg addr = do
    epoch <- siEpoch <$> getCurrentSlot
    richmen <- getRichmenSsc epoch
    maybe (pure False) (\r -> sscProcessMessage r msg addr) richmen

sendOurData
    :: (WorkMode SscGodTossing m)
    => GtMsgTag -> EpochIndex -> LocalSlotIndex -> StakeholderId -> m ()
sendOurData msgTag epoch kMultiplier ourId = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch kMultiplier
    logDebug $ sformat ("Announcing our "%build) msgTag
    let msg = InvMsg {imTag = msgTag, imKeys = pure ourId}
    sendToNeighborsSafe msg
    logDebug $ sformat ("Sent our " %build%" to neighbors") msgTag

-- | Generate new commitment and opening and use them for the current
-- epoch. 'prepareSecretToNewSlot' must be called before doing it.
--
-- Nothing is returned if node is not ready (usually it means that
-- node doesn't have recent enough blocks and needs to be
-- synchronized).
generateAndSetNewSecret
    :: (WorkMode SscGodTossing m, Bi Commitment)
    => SecretKey
    -> SlotId                         -- ^ Current slot
    -> m (Maybe (SignedCommitment, Opening))
generateAndSetNewSecret sk SlotId{..} = do
    richmen <- lrcActionOnEpochReason siEpoch
                   "couldn't get SSC richmen"
                   getRichmenSsc
    certs <- getGlobalCerts
    let noPsErr = panic "generateAndSetNewSecret: no participants"
    let ps = fromMaybe (panic noPsErr) . nonEmpty .
                map vcVssKey . mapMaybe (`lookup` certs) . toList $ richmen
    let threshold = vssThreshold $ length ps
    mPair <- runMaybeT (genCommitmentAndOpening threshold ps)
    tip <- getTip
    case mPair of
      Just (mkSignedCommitment sk siEpoch -> comm, op) ->
          Just (comm, op) <$ setSecret (toPublic sk, comm, op) tip
      _ -> do
        logError "Wrong participants list: can't deserialize"
        return Nothing

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
        wait $ for timeToWait
