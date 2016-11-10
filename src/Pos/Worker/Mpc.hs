-- | MPC processing related workers.

module Pos.Worker.Mpc
       (
         -- * Instances
         -- ** instance SscWorkersClass SscDynamicState
       ) where

import           Control.TimeWarp.Logging  (logDebug)
import           Control.TimeWarp.Logging  (logWarning)
import           Control.TimeWarp.Timed    (Microsecond, repeatForever, sec)
import qualified Data.HashMap.Strict       as HM (toList)
import           Data.List.NonEmpty        (nonEmpty)
import           Data.Tagged               (Tagged (..))
import           Formatting                (build, ords, sformat, (%))
import           Serokell.Util.Exceptions  ()
import           Universum

import           Pos.Communication.Methods (announceCommitment, announceCommitments,
                                            announceOpening, announceOpenings,
                                            announceShares, announceSharesMulti,
                                            announceVssCertificates)
import           Pos.Constants             (mpcTransmitterInterval)
import           Pos.Slotting              (getCurrentSlot)
import           Pos.Ssc.Class.Workers     (SscWorkersClass (..))
import           Pos.Ssc.DynamicState      (DSPayload (..), SscDynamicState,
                                            hasCommitment, hasOpening, hasShares,
                                            isCommitmentIdx, isOpeningIdx, isSharesIdx)
import           Pos.State                 (generateAndSetNewSecret, getGlobalMpcData,
                                            getLocalSscPayload, getOurCommitment,
                                            getOurOpening, getOurShares, getSecret)
import           Pos.Types                 (SlotId (..))
import           Pos.WorkMode              (WorkMode, getNodeContext, ncPublicKey,
                                            ncSecretKey, ncVssKeyPair)

instance SscWorkersClass SscDynamicState where
    sscOnNewSlot = Tagged mpcOnNewSlot
    sscWorkers = Tagged mpcWorkers

-- | Action which should be done when new slot starts.
mpcOnNewSlot :: WorkMode m => SlotId -> m ()
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
        commitmentInBlockchain <- hasCommitment ourPk <$> getGlobalMpcData
        return $ isCommitmentIdx siSlot && not commitmentInBlockchain
    when shouldSendCommitment $ do
        mbComm <- getOurCommitment
        whenJust mbComm $ \comm -> do
            announceCommitment ourPk comm
            logDebug "Sent commitment to neighbors"
    -- Send the opening
    shouldSendOpening <- do
        openingInBlockchain <- hasOpening ourPk <$> getGlobalMpcData
        return $ isOpeningIdx siSlot && not openingInBlockchain
    when shouldSendOpening $ do
        mbOpen <- getOurOpening
        whenJust mbOpen $ \open -> do
            announceOpening ourPk open
            logDebug "Sent opening to neighbors"
    -- Send decrypted shares that others have sent us
    shouldSendShares <- do
        -- TODO: here we assume that all shares are always sent as a whole
        -- package.
        sharesInBlockchain <- hasShares ourPk <$> getGlobalMpcData
        return $ isSharesIdx siSlot && not sharesInBlockchain
    when shouldSendShares $ do
        ourVss <- ncVssKeyPair <$> getNodeContext
        shares <- getOurShares ourVss
        unless (null shares) $ do
            announceShares ourPk shares
            logDebug "Sent shares to neighbors"

-- | All workers specific to MPC processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
mpcWorkers :: WorkMode m => [m ()]
mpcWorkers = [mpcTransmitter]

mpcTransmitter :: WorkMode m => m ()
mpcTransmitter =
    repeatForever mpcTransmitterInterval onError $
    do DSPayload {..} <- getLocalSscPayload =<< getCurrentSlot
       whenJust (nonEmpty $ HM.toList _mdCommitments) announceCommitments
       whenJust (nonEmpty $ HM.toList _mdOpenings) announceOpenings
       whenJust (nonEmpty $ HM.toList _mdShares) announceSharesMulti
       whenJust
           (nonEmpty $ HM.toList _mdVssCertificates)
           announceVssCertificates
  where
    onError e =
        mpcTransmitterInterval <$
        logWarning (sformat ("Error occured in mpcTransmitter: " %build) e)
