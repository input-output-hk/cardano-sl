{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Concurrent.STM           (readTVar)
import           Control.Lens                     (view, (%=), _2, _3)
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
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Methods        (sendToNeighborsSafe)
import           Pos.Constants                    (k, mpcSendInterval)
import           Pos.Context                      (getNodeContext, ncPublicKey,
                                                   ncSecretKey, ncSscContext, readRichmen)
import           Pos.Crypto                       (SecretKey, VssKeyPair, randomNumber,
                                                   runSecureRandom, toPublic)
import           Pos.Crypto.SecretSharing         (toVssPublicKey)
import           Pos.Crypto.Signing               (PublicKey, sign)
import           Pos.Slotting                     (getSlotStart, onNewSlot)
import           Pos.Ssc.Class.Workers            (SscWorkersClass (..))
import           Pos.Ssc.Extra.MonadLD            (sscRunLocalQuery, sscRunLocalUpdate)
import           Pos.Ssc.GodTossing.Functions     (genCommitmentAndOpening, getThreshold,
                                                   hasCommitment, hasOpening, hasShares,
                                                   isCommitmentIdx, isOpeningIdx,
                                                   isSharesIdx, mkSignedCommitment)
import           Pos.Ssc.GodTossing.LocalData     (ldCertificates, localOnNewSlot,
                                                   sscProcessMessage)
import           Pos.Ssc.GodTossing.SecretStorage (getSecret, prepareSecretToNewSlot,
                                                   setSecret)
import           Pos.Ssc.GodTossing.Shares        (getOurShares)
import           Pos.Ssc.GodTossing.Storage       (getGlobalCertificates,
                                                   gtGetGlobalState)
import           Pos.Ssc.GodTossing.Types         (Commitment, Opening, SignedCommitment,
                                                   SscGodTossing, VssCertificate (..),
                                                   gtcParticipateSsc, gtcVssKeyPair)
import           Pos.Ssc.GodTossing.Types.Message (DataMsg (..), InvMsg (..), MsgTag (..))
import           Pos.Types                        (EpochIndex, LocalSlotIndex, NodeId,
                                                   SlotId (..), Timestamp (..),
                                                   addressHash)
import           Pos.Util                         (asBinary)
import           Pos.WorkMode                     (WorkMode)

instance SscWorkersClass SscGodTossing where
    sscWorkers = Tagged [onStart, onNewSlotSsc]

-- CHECK: @onStart
-- #checkNSendOurCert
onStart :: forall m. (WorkMode SscGodTossing m, Bi DataMsg) => m ()
onStart = checkNSendOurCert

-- CHECK: @checkNSendOurCert
-- Checks whether 'our' VSS certificate has been announced
checkNSendOurCert :: forall m . (WorkMode SscGodTossing m, Bi DataMsg) => m ()
checkNSendOurCert = do
    (_, ourId) <- getOurPkAndId
    isCertInBlockhain <- member ourId <$> getGlobalCertificates
    if isCertInBlockhain then
       logDebug "Our VssCertificate has been already announced."
    else do
        logDebug "Our VssCertificate hasn't been announced yet, we will announce it now."
        ourVssCertificate <- getOurVssCertificate
        let msg = DMVssCertificate ourId ourVssCertificate
        -- [CSL-245]: do not catch all, catch something more concrete.
        (sendToNeighborsSafe msg >> logDebug "Announced our VssCertificate.")
            `catchAll` \e ->
            logError $ sformat ("Error announcing our VssCertificate: " % shown) e
  where
    getOurVssCertificate :: m VssCertificate
    getOurVssCertificate = do
        (ourPk, ourId) <- getOurPkAndId
        localCerts     <- sscRunLocalQuery $ view ldCertificates
        case lookup ourId localCerts of
          Just c  -> return c
          Nothing -> do
            ourSk         <- ncSecretKey <$> getNodeContext
            ourVssKeyPair <- getOurVssKeyPair
            let vssKey  = asBinary $ toVssPublicKey ourVssKeyPair
                ourCert = VssCertificate { vcVssKey     = vssKey
                                         , vcSignature  = sign ourSk vssKey
                                         , vcSigningKey = ourPk
                                         }
            sscRunLocalUpdate $ ldCertificates %= insert ourId ourCert
            return ourCert

getOurPkAndId
    :: WorkMode SscGodTossing m
    => m (PublicKey, NodeId)
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
    prepareSecretToNewSlot slotId
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
onNewSlotCommitment SlotId {..} = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    ourSk <- ncSecretKey <$> getNodeContext
    shouldCreateCommitment <- do
        secret <- getSecret
        return $ and [isCommitmentIdx siSlot, isNothing secret]
    when shouldCreateCommitment $ do
        logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
        generated <- generateAndSetNewSecret ourSk siEpoch
        case generated of
            Nothing -> logWarning "I failed to generate secret for Mpc"
            Just _ -> logDebug $
                sformat ("Generated secret for "%ords%" epoch") siEpoch
    shouldSendCommitment <- do
        commitmentInBlockchain <- hasCommitment ourId <$> gtGetGlobalState
        return $ isCommitmentIdx siSlot && not commitmentInBlockchain
    when shouldSendCommitment $ do
        mbComm <- fmap (view _2) <$> getSecret
        whenJust mbComm $ \comm -> do
            _ <- sscProcessMessage $ DMCommitment ourId comm
            sendOurData CommitmentMsg siEpoch 0 ourId

-- Openings-related part of new slot processing
onNewSlotOpening
    :: (WorkMode SscGodTossing m
       ,Bi VssCertificate
       ,Bi Opening
       ,Bi InvMsg
       ,Bi Commitment)
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
            _ <- sscProcessMessage $ DMOpening ourId open
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
            _ <- sscProcessMessage $ DMShares ourId lShares
            sendOurData SharesMsg siEpoch 4 ourId

sendOurData
    :: (WorkMode SscGodTossing m, Bi InvMsg)
    => MsgTag -> EpochIndex -> LocalSlotIndex -> NodeId -> m ()
sendOurData msgTag epoch kMultiplier ourId = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch kMultiplier
    logDebug $ sformat ("Announcing our "%build) msgTag
    let msg = InvMsg {imType = msgTag, imNodes = pure ourId}
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
    -> EpochIndex                         -- ^ Current epoch
    -> m (Maybe (SignedCommitment, Opening))
generateAndSetNewSecret sk epoch = do
    richmen <- readRichmen
    certs <- getGlobalCertificates
    let noPsErr = panic "generateAndSetNewSecret: no participants"
    let ps = fromMaybe (panic noPsErr) . nonEmpty .
                map vcVssKey . mapMaybe (`lookup` certs) . toList $ richmen
    let threshold = getThreshold $ length ps
    mPair <- runMaybeT (genCommitmentAndOpening threshold ps)
    case mPair of
      Just (mkSignedCommitment sk epoch -> comm, op) ->
          Just (comm, op) <$ setSecret (toPublic sk, comm, op)
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
    => MsgTag -> EpochIndex -> LocalSlotIndex -> m ()
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
