{-# LANGUAGE Rank2Types #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationListeners

       , handleSendProxySK
       , handleConfirmProxySK
       , handleCheckProxySKConfirmed
       ) where

import           Control.Lens              (to, (%~), (^.))
import qualified Data.HashMap.Strict       as HM
import           Data.Time.Clock           (getCurrentTime)
import           Formatting                (build, sformat, shown, (%))
import           System.Wlog               (logDebug, logInfo)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Communication.Methods (sendProxyConfirmSK, sendProxySecretKey)
import           Pos.Communication.Types   (MutSocketState, ResponseMode)
import           Pos.Context               (getNodeContext, ncPropagation, ncSecretKey)
import           Pos.Crypto                (ProxySecretKey, checkProxySecretKey,
                                            pdDelegatePk, pdDelegatePk, proxySign,
                                            proxyVerify)
import           Pos.DB.Misc               (addProxySecretKey, getProxySecretKeys)
import           Pos.Delegation.Logic      (ConfirmPSKVerdict (..), PSKVerdict (..),
                                            invalidateProxyCaches, processConfirmProxySk,
                                            processProxySecretKey, runDelegationStateM)
import           Pos.Delegation.Types      (CheckProxySKConfirmed (..),
                                            CheckProxySKConfirmedRes (..),
                                            ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model             (ListenerDHT (..), MonadDHTDialog, replyToNode)
import           Pos.Types                 (EpochIndex, ProxySKEpoch, ProxySigEpoch)
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
    logDebug $ sformat ("Got request to handle proxy secret key: "%build) pSk
    -- do it in worker once in ~sometimes instead of on every request
    curTime <- liftIO getCurrentTime
    runDelegationStateM $ invalidateProxyCaches curTime
    verdict <- processProxySecretKey pSk
    logResult verdict
    propagateSendProxySK verdict pSk
  where
    logResult PSKAdded =
        logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
    logResult verdict =
        logDebug $
        sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict

-- | Propagates proxy secret key depending on the decision
propagateSendProxySK
    :: (WorkMode ssc m)
    => PSKVerdict -> ProxySecretKey (EpochIndex, EpochIndex) -> m ()
propagateSendProxySK PSKUnrelated pSk = do
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating proxy secret key "%build) pSk
        sendProxySecretKey pSk
propagateSendProxySK PSKAdded pSk = do
    logDebug $ sformat ("Generating delivery proof and propagating it: "%build) pSk
    sk <- ncSecretKey <$> getNodeContext
    let proof = proxySign sk pSk pSk -- but still proving is nothing but fear
    sendProxyConfirmSK $ ConfirmProxySK pSk proof
propagateSendProxySK _ _ = pure ()


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
    => ConfirmPSKVerdict -> ConfirmProxySK -> m ()
propagateConfirmProxySK ConfirmPSKValid confPSK@(ConfirmProxySK pSk _) = do
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
    res <- undefined
    --res <- withProxyCaches $ \p -> pure (p ^. ncProxyConfCache . to (HM.member pSk), p)
    replyToNode $ CheckProxySKConfirmedRes res
