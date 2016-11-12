{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
-- | MPC processing related workers.

module Pos.Ssc.DynamicState.Worker where

import           Control.TimeWarp.Logging  (logDebug)
import           Control.TimeWarp.Logging  (logWarning)
import           Control.TimeWarp.Timed    (Microsecond, repeatForever, sec)
import qualified Data.HashMap.Strict       as HM (toList)
import           Data.Tagged               (Tagged (..))
import           Formatting                (build, ords, sformat, (%))
import           Serokell.Util.Exceptions  ()
import           Universum

import           Pos.Ssc.DynamicState.Types    (SendSsc (..))
import           Pos.DHT                   (sendToNeighbors)
import           Pos.Slotting              (getCurrentSlot)
import           Pos.Ssc.Class.Workers     (SscWorkersClass (..))
import           Pos.Ssc.DynamicState.Types (DSPayload (..),
                                            hasCommitment, hasOpening, hasShares)
import           Pos.State                 (getGlobalMpcData, getLocalSscPayload)
import           Pos.Ssc.DynamicState.State (generateAndSetNewSecret, getOurCommitment,
                                             getOurOpening, getOurShares, getSecret)
import           Pos.Ssc.DynamicState.Instance (SscDynamicState)
import           Pos.Ssc.DynamicState.Base     (isCommitmentIdx, isOpeningIdx, isSharesIdx)
import           Pos.Types                 (SlotId (..))
import           Pos.WorkMode              (WorkMode, getNodeContext, ncPublicKey,
                                            ncSecretKey, ncVssKeyPair)
import           Pos.Crypto                (PublicKey, Share)
import           Pos.Ssc.DynamicState.Base (SignedCommitment, VssCertificate, Opening)
import           Pos.Communication.Methods (sendToNeighborsSafe)
import           Serokell.Util.Text        (mapJson)

instance SscWorkersClass SscDynamicState where
    sscOnNewSlot = Tagged mpcOnNewSlot
    sscWorkers = Tagged mpcWorkers

-- TODO: add statlogging for everything, see e.g. announceTxs
announceCommitment :: WorkMode ssc m => PublicKey -> SignedCommitment -> m ()
announceCommitment pk comm = do
    -- TODO: show the commitment
    logDebug $ sformat
        ("Announcing "%build%"'s commitment to others: <TODO SHOW COMM>") pk
    sendToNeighborsSafe $ SendCommitment pk comm

announceOpening :: WorkMode ssc m => PublicKey -> Opening -> m ()
announceOpening pk open = do
    logDebug $ sformat
        ("Announcing "%build%"'s opening to others: "%build) pk open
    sendToNeighborsSafe $ SendOpening pk open

announceShares :: WorkMode ssc m => PublicKey -> HashMap PublicKey Share -> m ()
announceShares pk shares = do
    logDebug $ sformat
        ("Announcing "%build%"'s shares to others:\n"%mapJson) pk shares
    sendToNeighborsSafe $ SendShares pk shares

announceVssCertificate :: WorkMode ssc m => PublicKey -> VssCertificate -> m ()
announceVssCertificate pk cert = do
    -- TODO: show the certificate
    logDebug $ sformat
        ("Announcing "%build%"'s VSS certificate to others: <TODO SHOW CERT>") pk
    void . sendToNeighbors $ SendVssCertificate pk cert
-- | Action which should be done when new slot starts.
mpcOnNewSlot :: WorkMode SscDynamicState m => SlotId -> m ()
mpcOnNewSlot SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    ourSk <- ncSecretKey <$> getNodeContext
    -- TODO: should we randomise sending times to avoid the situation when
    -- the network becomes overwhelmed with everyone's messages?

    -- If we haven't yet, generate a new commitment and opening for MPC; send
    -- the commitment.
    shouldCreateCommitment <- do
        secret <- getSecret
        return $ isCommitmentIdx siSlot && isNothing secret
    when shouldCreateCommitment $ do
        logDebug $ sformat ("Generating secret for "%ords%" epoch") siEpoch
        generated <- generateAndSetNewSecret ourSk siEpoch
        case generated of
            Nothing -> logWarning "I failed to generate secret for Mpc"
            Just _ -> logDebug $
                sformat ("Generated secret for "%ords%" epoch") siEpoch
    shouldSendCommitment <- do
        commitmentInBlockchain <- hasCommitment ourPk <$> getGlobalMpcData @SscDynamicState
        return $ isCommitmentIdx siSlot && not commitmentInBlockchain
    when shouldSendCommitment $ do
        mbComm <- getOurCommitment
        whenJust mbComm $ \comm -> do
            void . sendToNeighbors $ SendCommitment ourPk comm
            logDebug "Sent commitment to neighbors"
    -- Send the opening
    shouldSendOpening <- do
        openingInBlockchain <- hasOpening ourPk <$> getGlobalMpcData @SscDynamicState
        return $ isOpeningIdx siSlot && not openingInBlockchain
    when shouldSendOpening $ do
        mbOpen <- getOurOpening
        whenJust mbOpen $ \open -> do
            void . sendToNeighbors $ SendOpening ourPk open
            logDebug "Sent opening to neighbors"
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        -- TODO: here we assume that all shares are always sent as a whole
        -- package.
        sharesInBlockchain <- hasShares ourPk <$> getGlobalMpcData @SscDynamicState
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- ncVssKeyPair <$> getNodeContext
        shares <- getOurShares ourVss
        unless (null shares) $ do
            void . sendToNeighbors $ SendShares ourPk shares
            logDebug "Sent shares to neighbors"

-- | All workers specific to MPC processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
mpcWorkers :: WorkMode SscDynamicState m => [m ()]
mpcWorkers = [mpcTransmitter]

mpcTransmitterInterval :: Microsecond
mpcTransmitterInterval = sec 2

mpcTransmitter :: WorkMode SscDynamicState m => m ()
mpcTransmitter =
    repeatForever mpcTransmitterInterval onError $
    do DSPayload{..} <- getLocalSscPayload @SscDynamicState =<< getCurrentSlot
       mapM_ (uncurry announceCommitment) $ HM.toList _mdCommitments
       mapM_ (uncurry announceOpening) $ HM.toList _mdOpenings
       mapM_ (uncurry announceShares) $ HM.toList _mdShares
       mapM_ (uncurry announceVssCertificate) $ HM.toList _mdVssCertificates
  where
    onError e =
        mpcTransmitterInterval <$
        logWarning (sformat ("Error occured in mpcTransmitter: "%build) e)
