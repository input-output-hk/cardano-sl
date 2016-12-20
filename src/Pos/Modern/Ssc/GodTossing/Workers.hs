{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Instance of SscWorkersClass.

module Pos.Modern.Ssc.GodTossing.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Concurrent.STM                        (readTVar)
import           Control.Lens                                  (view, (%=), _2, _3)
import           Control.Monad.Trans.Maybe                     (runMaybeT)
import           Control.TimeWarp.Timed                        (Microsecond, Millisecond,
                                                                currentTime, for, wait)
import           Data.HashMap.Strict                           (insert, lookup, member)
import           Data.Tagged                                   (Tagged (..))
import           Data.Time.Units                               (convertUnit)
import           Formatting                                    (build, ords, sformat,
                                                                shown, (%))
import           Serokell.Util.Exceptions                      ()
import           System.Wlog                                   (logDebug, logError,
                                                                logWarning)
import           Universum

import           Pos.Binary.Class                              (Bi)
import           Pos.Communication.Methods                     (sendToNeighborsSafe)
import           Pos.Constants                                 (k, mpcSendInterval)
import           Pos.Context                                   (getNodeContext,
                                                                ncPublicKey, ncSecretKey,
                                                                ncSscContext)
import           Pos.Context                                   (getNodeContext)
import           Pos.Context.Context                           (ncSscParticipants)
import           Pos.Crypto                                    (SecretKey, VssKeyPair,
                                                                randomNumber,
                                                                runSecureRandom, toPublic)
import           Pos.Crypto.SecretSharing                      (toVssPublicKey)
import           Pos.Crypto.Signing                            (PublicKey, sign)
import           Pos.Modern.Ssc.GodTossing.Functions           (getThreshold,
                                                                hasCommitment, hasOpening,
                                                                hasShares)
import           Pos.Modern.Ssc.GodTossing.Helpers             (getOurShares)
import           Pos.Modern.Ssc.GodTossing.LocalData.LocalData (localOnNewSlot,
                                                                sscProcessMessage)
import           Pos.Modern.Ssc.GodTossing.Storage.Storage     (getGlobalCertificates)
import           Pos.Slotting                                  (getSlotStart, onNewSlot)
import           Pos.Ssc.Class.Helpers                         (SscHelpersClassM)
import           Pos.Ssc.Class.LocalData                       (MonadSscLDM,
                                                                sscRunLocalQuery,
                                                                sscRunLocalUpdate)
import           Pos.Ssc.Class.Storage                         (getGlobalState)
import           Pos.Ssc.Class.Workers                         (SscWorkersClass (..))
import           Pos.Ssc.GodTossing.Functions                  (genCommitmentAndOpening,
                                                                genCommitmentAndOpening,
                                                                isCommitmentIdx,
                                                                isOpeningIdx, isSharesIdx,
                                                                mkSignedCommitment)
import           Pos.Ssc.GodTossing.LocalData.Types            (gtLocalCertificates)
import           Pos.Ssc.GodTossing.SecretStorage.State        (checkpointSecret,
                                                                getSecret,
                                                                prepareSecretToNewSlot,
                                                                setSecret)
import           Pos.Ssc.GodTossing.Types.Base                 (Commitment, Opening,
                                                                SignedCommitment,
                                                                VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Instance             ()
import           Pos.Ssc.GodTossing.Types.Message              (DataMsg (..), InvMsg (..),
                                                                MsgTag (..))
import           Pos.Ssc.GodTossing.Types.Type                 (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types                (GtPayload, GtProof,
                                                                gtcParticipateSsc,
                                                                gtcVssKeyPair)
import           Pos.Types                                     (Address (..), EpochIndex,
                                                                LocalSlotIndex,
                                                                Participants, SlotId (..),
                                                                Timestamp (..),
                                                                makePubKeyAddress)
import           Pos.Util                                      (asBinary)
import           Pos.WorkMode                                  (WorkMode)

type MyWorkMode ssc m = (WorkMode ssc m, MonadSscLDM ssc m, SscHelpersClassM ssc)

instance (Bi VssCertificate
         ,Bi Opening
         ,Bi Commitment
         ,Bi GtPayload
         ,Bi DataMsg
         ,Bi InvMsg
         ,Bi GtProof) =>
         SscWorkersClass SscGodTossing where
    --sscWorkers = Tagged [onStart, onNewSlotSsc]
    sscWorkers = Tagged []

-- CHECK: @onStart
-- Checks whether 'our' VSS certificate has been announced
onStart :: forall m. (WorkMode SscGodTossing m, Bi DataMsg) => m ()
onStart = checkNSendOurCert

checkNSendOurCert :: forall m . (WorkMode SscGodTossing m, Bi DataMsg) => m ()
checkNSendOurCert = do
    (_, ourAddr) <- getOurPkAndAddr
    isCertInBlockhain <- member ourAddr <$> getGlobalCertificates
    if isCertInBlockhain then
       logDebug "Our VssCertificate has been already announced."
    else do
        logDebug "Our VssCertificate hasn't been announced yet, we will announce it now."
        ourVssCertificate <- getOurVssCertificate
        let msg = DMVssCertificate ourAddr ourVssCertificate
        -- [CSL-245]: do not catch all, catch something more concrete.
        (sendToNeighborsSafe msg >> logDebug "Announced our VssCertificate.")
            `catchAll` \e ->
            logError $ sformat ("Error announcing our VssCertificate: " % shown) e
  where
    getOurVssCertificate :: m VssCertificate
    getOurVssCertificate = do
        (ourPk, ourAddr) <- getOurPkAndAddr
        localCerts       <- sscRunLocalQuery $ view gtLocalCertificates
        case lookup ourAddr localCerts of
          Just c  -> return c
          Nothing -> do
            ourSk         <- ncSecretKey <$> getNodeContext
            ourVssKeyPair <- getOurVssKeyPair
            let vssKey  = asBinary $ toVssPublicKey ourVssKeyPair
                ourCert = VssCertificate { vcVssKey     = vssKey
                                         , vcSignature  = sign ourSk vssKey
                                         , vcSigningKey = ourPk
                                         }
            sscRunLocalUpdate $ gtLocalCertificates %= insert ourAddr ourCert
            return ourCert

getOurPkAndAddr :: WorkMode SscGodTossing m => m (PublicKey, Address)
getOurPkAndAddr = do
    ourPk <- ncPublicKey <$> getNodeContext
    return (ourPk, makePubKeyAddress ourPk)

getOurVssKeyPair :: WorkMode SscGodTossing m => m VssKeyPair
getOurVssKeyPair = gtcVssKeyPair . ncSscContext <$> getNodeContext

-- CHECK: @onNewSlotSsc
-- Checks whether 'our' VSS certificate has been announced
onNewSlotSsc
    :: ( MyWorkMode SscGodTossing m
       , Bi Commitment
       , Bi VssCertificate
       , Bi Opening
       , Bi InvMsg
       , Bi DataMsg)
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
        checkpointSecret

-- Commitments-related part of new slot processing
onNewSlotCommitment
    :: (MyWorkMode SscGodTossing m
       ,Bi Commitment
       ,Bi VssCertificate
       ,Bi Opening
       ,Bi InvMsg)
    => SlotId -> m ()
onNewSlotCommitment SlotId {..} = do
    ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
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
        commitmentInBlockchain <- hasCommitment ourAddr <$> getGlobalState
        return $ isCommitmentIdx siSlot && not commitmentInBlockchain
    when shouldSendCommitment $ do
        mbComm <- fmap (view _2) <$> getSecret
        whenJust mbComm $ \comm -> do
            _ <- sscProcessMessage $ DMCommitment ourAddr comm
            sendOurData CommitmentMsg siEpoch 0 ourAddr

-- Openings-related part of new slot processing
onNewSlotOpening
    :: (MyWorkMode SscGodTossing m
       ,Bi VssCertificate
       ,Bi Opening
       ,Bi InvMsg
       ,Bi Commitment)
    => SlotId -> m ()
onNewSlotOpening SlotId {..} = do
    ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
    shouldSendOpening <- do
        globalData <- getGlobalState
        let openingInBlockchain = hasOpening ourAddr globalData
        let commitmentInBlockchain = hasCommitment ourAddr globalData
        return $ and [ isOpeningIdx siSlot
                     , not openingInBlockchain
                     , commitmentInBlockchain]
    when shouldSendOpening $ do
        mbOpen <- fmap (view _3) <$> getSecret
        whenJust mbOpen $ \open -> do
            _ <- sscProcessMessage $ DMOpening ourAddr open
            sendOurData OpeningMsg siEpoch 2 ourAddr

-- Shares-related part of new slot processing
onNewSlotShares
    :: (MyWorkMode SscGodTossing m
       ,Bi VssCertificate
       ,Bi InvMsg
       ,Bi Opening
       ,Bi Commitment)
    => SlotId -> m ()
onNewSlotShares SlotId {..} = do
    ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        -- [CSL-203]: here we assume that all shares are always sent
        -- as a whole package.
        sharesInBlockchain <- hasShares ourAddr <$> getGlobalState
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- gtcVssKeyPair . ncSscContext <$> getNodeContext
        shares <- getOurShares ourVss
        let lShares = fmap asBinary shares
        unless (null shares) $ do
            _ <- sscProcessMessage $ DMShares ourAddr lShares
            sendOurData SharesMsg siEpoch 4 ourAddr

sendOurData
    :: (WorkMode SscGodTossing m, Bi InvMsg)
    => MsgTag -> EpochIndex -> LocalSlotIndex -> Address -> m ()
sendOurData msgTag epoch kMultiplier ourAddr = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch kMultiplier
    logDebug $ sformat ("Announcing our "%build) msgTag
    let msg = InvMsg {imType = msgTag, imKeys = pure ourAddr}
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
    -- It should be safe here to perform 2 operations (get and set)
    -- which aren't grouped into a single transaction here, because if
    -- getParticipants returns 'Just res' it will always return 'Just
    -- res' unless key assumption is broken. But if it's broken,
    -- nothing else matters.
    ps <- takeParticipants
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

takeParticipants :: WorkMode SscGodTossing m => m Participants
takeParticipants = getNodeContext >>= liftIO . readMVar . ncSscParticipants
