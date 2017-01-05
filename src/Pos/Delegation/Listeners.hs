{-# LANGUAGE Rank2Types #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationListeners

       , handleSendProxySK
       , handleConfirmProxySK
       , handleCheckProxySKConfirmed
       ) where

import           Data.Time.Clock           (getCurrentTime)
import           Formatting                (build, sformat, shown, (%))
import           System.Wlog               (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Communication.Methods (sendProxyConfirmSK, sendProxySecretKey)
import           Pos.Communication.Types   (MutSocketState, ResponseMode)
import           Pos.Context               (getNodeContext, ncPropagation, ncSecretKey)
import           Pos.Crypto                (PublicKey, proxySign)
import           Pos.Delegation.Logic      (ConfirmPskEpochVerdict (..),
                                            PskEpochVerdict (..), invalidateProxyCaches,
                                            isProxySKConfirmed, processConfirmProxySk,
                                            processProxySKEpoch, runDelegationStateAction)
import           Pos.Delegation.Types      (CheckProxySKConfirmed (..),
                                            CheckProxySKConfirmedRes (..),
                                            ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model             (ListenerDHT (..), MonadDHTDialog, replyToNode)
import           Pos.Types                 (ProxySKEpoch, ProxySKSimple)
import           Pos.WorkMode              (WorkMode)


-- | Listeners for requests related to delegation processing.
delegationListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
delegationListeners =
    [ ListenerDHT handleSendProxySK
    , ListenerDHT handleConfirmProxySK
    , ListenerDHT handleCheckProxySKConfirmed
    ]

----------------------------------------------------------------------------
-- PSKs propagation
----------------------------------------------------------------------------

-- | Handler 'SendProxySK' event.
handleSendProxySK
    :: forall ssc m.
       (ResponseMode ssc m)
    => SendProxySK -> m ()
handleSendProxySK (SendProxySKEpoch pSk) = do
    logDebug $ sformat ("Got request to handle lightweight psk: "%build) pSk
    -- do it in worker once in ~sometimes instead of on every request
    curTime <- liftIO getCurrentTime
    runDelegationStateAction $ invalidateProxyCaches curTime
    verdict <- processProxySKEpoch pSk
    logResult verdict
    propagateProxySKEpoch verdict pSk
  where
    logResult PEAdded =
        logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
    logResult verdict =
        logDebug $
        sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict
handleSendProxySK (SendProxySKSimple pSk) = do
    logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
    notImplemented

-- | Propagates lightweight PSK depending on the decision.
propagateProxySKEpoch
    :: (WorkMode ssc m)
    => PskEpochVerdict -> ProxySKEpoch -> m ()
propagateProxySKEpoch PEUnrelated pSk = do
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating proxy secret key "%build) pSk
        sendProxySecretKey pSk
propagateProxySKEpoch PEAdded pSk = do
    logDebug $ sformat ("Generating delivery proof and propagating it: "%build) pSk
    sk <- ncSecretKey <$> getNodeContext
    let proof = proxySign sk pSk pSk -- but still proving is nothing but fear
    sendProxyConfirmSK $ ConfirmProxySK pSk proof
propagateProxySKEpoch _ _ = pure ()


----------------------------------------------------------------------------
-- Light PSKs backpropagation (confirmations)
----------------------------------------------------------------------------

handleConfirmProxySK
    :: forall ssc m.
       (ResponseMode ssc m)
    => ConfirmProxySK -> m ()
handleConfirmProxySK o@(ConfirmProxySK pSk proof) = do
    logDebug $ sformat ("Got request to handle confirmation for psk: "%build) pSk
    verdict <- processConfirmProxySk pSk proof
    propagateConfirmProxySK verdict o

propagateConfirmProxySK
    :: (WorkMode ssc m)
    => ConfirmPskEpochVerdict -> ConfirmProxySK -> m ()
propagateConfirmProxySK CPValid confPSK@(ConfirmProxySK pSk _) = do
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating psk confirmation for psk: "%build) pSk
        sendProxyConfirmSK confPSK
propagateConfirmProxySK _ _ = pure ()

handleCheckProxySKConfirmed
    :: forall ssc m.
       (ResponseMode ssc m)
    => CheckProxySKConfirmed -> m ()
handleCheckProxySKConfirmed (CheckProxySKConfirmed pSk) = do
    logDebug $ sformat ("Got request to check if psk: "%build%" was delivered.") pSk
    res <- runDelegationStateAction $ isProxySKConfirmed pSk
    replyToNode $ CheckProxySKConfirmedRes res
