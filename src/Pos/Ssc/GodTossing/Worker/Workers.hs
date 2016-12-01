{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Worker.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Concurrent.STM                 (readTVar)
import           Control.Lens                           (view, _2, _3)
import           Control.Monad.Trans.Maybe              (runMaybeT)
import           Control.TimeWarp.Timed                 (Microsecond, Millisecond,
                                                         currentTime, for, wait)
import           Data.Tagged                            (Tagged (..))
import           Data.Time.Units                        (convertUnit)
import           Formatting                             (build, ords, sformat, shown, (%))
import           Serokell.Util.Exceptions               ()
import           System.Wlog                            (logDebug, logError, logWarning)
import           Universum

import           Pos.Communication.Methods              (sendToNeighborsSafe)
import           Pos.Constants                          (k, mpcSendInterval)
import           Pos.Crypto                             (PublicKey, SecretKey,
                                                         randomNumber, runSecureRandom,
                                                         toPublic)
import           Pos.Slotting                           (getSlotStart, onNewSlot)
import           Pos.Ssc.Class.Workers                  (SscWorkersClass (..))
import           Pos.Ssc.GodTossing.Functions           (genCommitmentAndOpening,
                                                         genCommitmentAndOpening,
                                                         hasCommitment, hasOpening,
                                                         hasShares, isCommitmentIdx,
                                                         isOpeningIdx, isSharesIdx,
                                                         mkSignedCommitment)
import           Pos.Ssc.GodTossing.LocalData.LocalData (localOnNewSlot,
                                                         sscProcessMessage)
import           Pos.Ssc.GodTossing.SecretStorage.State (checkpointSecret, getSecret,
                                                         prepareSecretToNewSlot,
                                                         setSecret)
import           Pos.Ssc.GodTossing.Types.Base          (Opening, SignedCommitment)
import           Pos.Ssc.GodTossing.Types.Instance      ()
import           Pos.Ssc.GodTossing.Types.Message       (DataMsg (..), InvMsg (..),
                                                         MsgTag (..))
import           Pos.Ssc.GodTossing.Types.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types         (gtcParticipateSsc, gtcVssKeyPair)
import           Pos.State                              (getGlobalMpcData, getOurShares,
                                                         getParticipants, getThreshold)
import           Pos.Types                              (EpochIndex, LocalSlotIndex,
                                                         SlotId (..), Timestamp (..))
import           Pos.Util                               (serialize)
import           Pos.WorkMode                           (WorkMode, getNodeContext,
                                                         ncPublicKey, ncSecretKey,
                                                         ncSscContext)
instance SscWorkersClass SscGodTossing where
    sscWorkers = Tagged [onNewSlotSsc]

onNewSlotSsc :: WorkMode SscGodTossing m => m ()
onNewSlotSsc = onNewSlot True $ \slotId-> do
    localOnNewSlot slotId
    prepareSecretToNewSlot slotId
    onNewSlotCommitment slotId
    onNewSlotOpening slotId
    onNewSlotShares slotId
    checkpointSecret

-- Commitments-related part of new slot processing
onNewSlotCommitment :: WorkMode SscGodTossing m => SlotId -> m ()
onNewSlotCommitment SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    ourSk <- ncSecretKey <$> getNodeContext
    shouldCreateCommitment <- do
        participationEnabled <- getNodeContext >>=
            atomically . readTVar . gtcParticipateSsc . ncSscContext
        secret <- getSecret
        return $
            and [participationEnabled, isCommitmentIdx siSlot, isNothing secret]
    when shouldCreateCommitment $ do
        logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
        generated <- generateAndSetNewSecret ourSk siEpoch
        case generated of
            Nothing -> logWarning "I failed to generate secret for Mpc"
            Just _ -> logDebug $
                sformat ("Generated secret for "%ords%" epoch") siEpoch
    shouldSendCommitment <- do
        commitmentInBlockchain <- hasCommitment ourPk <$> getGlobalMpcData
        return $ isCommitmentIdx siSlot && not commitmentInBlockchain
    when shouldSendCommitment $ do
        mbComm <- fmap (view _2) <$> getSecret
        whenJust mbComm $ \comm -> do
            _ <- sscProcessMessage $ DMCommitment ourPk comm
            sendOurData CommitmentMsg siEpoch 0 ourPk

-- Openings-related part of new slot processing
onNewSlotOpening :: WorkMode SscGodTossing m => SlotId -> m ()
onNewSlotOpening SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    shouldSendOpening <- do
        globalData <- getGlobalMpcData
        let openingInBlockchain = hasOpening ourPk globalData
        let commitmentInBlockchain = hasCommitment ourPk globalData
        return $ and [ isOpeningIdx siSlot
                     , not openingInBlockchain
                     , commitmentInBlockchain]
    when shouldSendOpening $ do
        mbOpen <- fmap (view _3) <$> getSecret
        whenJust mbOpen $ \open -> do
            _ <- sscProcessMessage $ DMOpening ourPk open
            sendOurData OpeningMsg siEpoch 2 ourPk

-- Shares-related part of new slot processing
onNewSlotShares :: WorkMode SscGodTossing m => SlotId -> m ()
onNewSlotShares SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        -- [CSL-203]: here we assume that all shares are always sent
        -- as a whole package.
        sharesInBlockchain <- hasShares ourPk <$> getGlobalMpcData
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- gtcVssKeyPair . ncSscContext <$> getNodeContext
        shares <- getOurShares ourVss
        let lShares = fmap serialize shares
        unless (null shares) $ do
            _ <- sscProcessMessage $ DMShares ourPk lShares
            sendOurData SharesMsg siEpoch 4 ourPk

sendOurData
    :: WorkMode SscGodTossing m
    => MsgTag -> EpochIndex -> LocalSlotIndex -> PublicKey -> m ()
sendOurData msgTag epoch kMultiplier ourPk = do
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgTag epoch kMultiplier
    logDebug $ sformat ("Announcing our "%build) msgTag
    let msg = InvMsg {imType = msgTag, imKeys = pure ourPk}
    sendToNeighborsSafe msg
    logDebug $ sformat ("Sent our " %build%" to neighbors") msgTag

-- | Generate new commitment and opening and use them for the current
-- epoch. 'prepareSecretToNewSlot' must be called before doing it.
--
-- Nothing is returned if node is not ready (usually it means that
-- node doesn't have recent enough blocks and needs to be
-- synchronized).
generateAndSetNewSecret
    :: WorkMode SscGodTossing m
    => SecretKey
    -> EpochIndex                         -- ^ Current epoch
    -> m (Maybe (SignedCommitment, Opening))
generateAndSetNewSecret sk epoch = do
    -- It should be safe here to perform 2 operations (get and set)
    -- which aren't grouped into a single transaction here, because if
    -- getParticipants returns 'Just res' it will always return 'Just
    -- res' unless key assumption is broken. But if it's broken,
    -- nothing else matters.
    participants <- getParticipants epoch
    case participants of
        Nothing -> return Nothing
        Just ps -> do
            let threshold = getThreshold $ length ps
            mPair <- runMaybeT (genCommitmentAndOpening threshold ps)
            case mPair of
              Just (mkSignedCommitment sk epoch -> comm, op) ->
                  Just (comm, op) <$ setSecret (toPublic sk, comm, op)
              _ -> do
                logError "Wrong participants list: can't deserialize"
                return Nothing

-- pathToSecret :: WorkMode SscGodTossing m => m (Maybe FilePath)
-- pathToSecret = fmap (</> "secret") <$> (ncDbPath <$> getNodeContext)

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
