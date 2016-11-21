{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

-- | Instance of SscWorkersClass.

module Pos.Ssc.GodTossing.Worker.Workers
       ( -- * Instances
         -- ** instance SscWorkersClass SscGodTossing
       ) where

import           Control.Lens                            (view, _2, _3)
import           Control.TimeWarp.Timed                  (Microsecond, Millisecond,
                                                          currentTime, for, repeatForever,
                                                          wait)
import qualified Data.HashMap.Strict                     as HM (toList)
import           Data.List.NonEmpty                      (nonEmpty)
import           Data.Tagged                             (Tagged (..))
import           Data.Time.Units                         (convertUnit)
import           Formatting                              (build, ords, sformat, shown,
                                                          stext, (%))
import           Serokell.Util.Exceptions                ()
import           System.FilePath                         ((</>))
import           System.Wlog                             (logDebug, logWarning)
import           Universum

import           Pos.Communication.Methods               (announceSsc)
import           Pos.Constants                           (k, mpcSendInterval,
                                                          sscTransmitterInterval)
import           Pos.Crypto                              (SecretKey, randomNumber,
                                                          runSecureRandom, toPublic)
import           Pos.Slotting                            (getCurrentSlot, getSlotStart)
import           Pos.Ssc.Class.Workers                   (SscWorkersClass (..))
import           Pos.Ssc.GodTossing.Announce             (announceCommitments,
                                                          announceOpenings,
                                                          announceSharesMulti,
                                                          announceVssCertificates)
import           Pos.Ssc.GodTossing.Types.Base           (Opening, SignedCommitment,
                                                          genCommitmentAndOpening,
                                                          genCommitmentAndOpening,
                                                          isCommitmentIdx, isOpeningIdx,
                                                          isSharesIdx, mkSignedCommitment)
import           Pos.Ssc.GodTossing.Types.Type           (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types          (GtMessage (..), GtPayload (..),
                                                          hasCommitment, hasOpening,
                                                          hasShares)
import           Pos.Ssc.GodTossing.Worker.SecretStorage (checkpoint, getSecret,
                                                          prepareSecretToNewSlot,
                                                          setSecret)
import           Pos.Ssc.GodTossing.Worker.Types         (GtSecret)
import           Pos.State                               (getGlobalMpcData,
                                                          getLocalSscPayload,
                                                          getOurShares, getParticipants,
                                                          getThreshold, processSscMessage)
import           Pos.Types                               (EpochIndex, LocalSlotIndex,
                                                          SlotId (..), Timestamp (..))
import           Pos.WorkMode                            (WorkMode, getNodeContext,
                                                          ncDbPath, ncPublicKey,
                                                          ncSecretKey, ncVssKeyPair)

instance SscWorkersClass SscGodTossing where
    sscOnNewSlot = Tagged onNewSlot
    sscWorkers = Tagged [sscTransmitter]

onNewSlot :: WorkMode SscGodTossing m => SlotId -> m ()
onNewSlot slotId = do
    prepareTokenToNewSlot slotId
    onNewSlotCommitment slotId
    onNewSlotOpening slotId
    onNewSlotShares slotId
    createCheckpoint slotId

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
    => Text -> EpochIndex -> LocalSlotIndex -> m ()
waitUntilSend msgName epoch kMultiplier = do
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
            sformat ("Waiting for "%shown%" before sending "%stext)
                ttwMillisecond msgName
        wait $ for timeToWait

createCheckpoint :: WorkMode SscGodTossing m => SlotId -> m ()
createCheckpoint SlotId {..} = pathToSecret >>= checkpoint

-- Commitments-related part of new slot processing
onNewSlotCommitment :: WorkMode SscGodTossing m => SlotId -> m ()
onNewSlotCommitment SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    ourSk <- ncSecretKey <$> getNodeContext
    shouldCreateCommitment <- do
        secret <- getToken
        return $ isCommitmentIdx siSlot && isNothing secret
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
        mbComm <- fmap (view _2) <$> getToken
        whenJust mbComm $
            onSendSomething siEpoch "commitment" 0 .
            DSCommitments . pure . (ourPk,)

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
        mbOpen <- fmap (view _3) <$> getToken
        whenJust mbOpen $
            onSendSomething siEpoch "opening" 2 . DSOpenings . pure . (ourPk,)

-- Shares-related part of new slot processing
onNewSlotShares :: WorkMode SscGodTossing m => SlotId -> m ()
onNewSlotShares SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        -- TODO: here we assume that all shares are always sent as a whole
        -- package.
        sharesInBlockchain <- hasShares ourPk <$> getGlobalMpcData
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- ncVssKeyPair <$> getNodeContext
        shares <- getOurShares ourVss
        let msg = DSSharesMulti $ pure (ourPk, shares)
        unless (null shares) $
            onSendSomething siEpoch "shares" 4 msg

onSendSomething
    :: WorkMode SscGodTossing m
    => EpochIndex -> Text -> LocalSlotIndex -> GtMessage -> m ()
onSendSomething epoch msgName kMultiplier msg = do
    () <$ processSscMessage msg
    -- Note: it's not necessary to create a new thread here, because
    -- in one invocation of onNewSlot we can't process more than one
    -- type of message.
    waitUntilSend msgName epoch kMultiplier
    logDebug $ "Announcing our " `mappend` msgName
    announceSsc msg
    logDebug $ sformat ("Sent our "%stext%" to neighbors") msgName

sscTransmitter :: WorkMode SscGodTossing m => m ()
sscTransmitter =
    repeatForever sscTransmitterInterval onError $
    do GtPayload {..} <- getLocalSscPayload =<< getCurrentSlot
       whenJust (nonEmpty $ HM.toList _mdCommitments) announceCommitments
       whenJust (nonEmpty $ HM.toList _mdOpenings) announceOpenings
       whenJust (nonEmpty $ HM.toList _mdShares) announceSharesMulti
       whenJust
           (nonEmpty $ HM.toList _mdVssCertificates)
           announceVssCertificates
  where
    onError e =
        sscTransmitterInterval <$
        logWarning (sformat ("Error occured in sscTransmitter: " %build) e)

-- | Generate new commitment and opening and use them for the current
-- epoch. Assumes that the genesis block has already been generated and
-- processed by MPC (when the genesis block is processed, the secret is
-- cleared) (otherwise 'generateNewSecret' will fail because 'A.SetSecret'
-- won't set the secret if there's one already).
-- Nothing is returned if node is not ready.
generateAndSetNewSecret
    :: WorkMode SscGodTossing m
    => SecretKey
    -> EpochIndex                         -- ^ Current epoch
    -> m (Maybe (SignedCommitment, Opening))
generateAndSetNewSecret sk epoch = do
    -- TODO: I think it's safe here to perform 3 operations which aren't
    -- grouped into a single transaction here, but I'm still a bit nervous.
    threshold <- getThreshold epoch
    participants <- getParticipants epoch
    case (,) <$> threshold <*> participants of
        Nothing -> return Nothing
        Just (th, ps) -> do
            (comm, op) <-
                first (mkSignedCommitment sk epoch) <$>
                genCommitmentAndOpening th ps
            Just (comm, op) <$ setToken (toPublic sk, comm, op)

pathToSecret :: WorkMode SscGodTossing m => m (Maybe FilePath)
pathToSecret = fmap (</> "secret") <$> (ncDbPath <$> getNodeContext)

setToken :: WorkMode SscGodTossing m => GtSecret -> m ()
setToken secret = pathToSecret >>= flip setSecret secret

getToken :: WorkMode SscGodTossing m => m (Maybe GtSecret)
getToken = pathToSecret >>= getSecret

prepareTokenToNewSlot :: WorkMode SscGodTossing m => SlotId -> m ()
prepareTokenToNewSlot slotId = pathToSecret >>= flip prepareSecretToNewSlot slotId
