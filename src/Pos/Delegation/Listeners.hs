{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Pos.Communication.Types   (MutPeerState, ResponseMode)
import           Pos.Context               (getNodeContext, ncPropagation, ncSecretKey)
import           Pos.Crypto                (proxySign)
import           Pos.Delegation.Logic      (ConfirmPSKVerdict (..), PSKVerdict (..),
                                            invalidateProxyCaches, isProxySKConfirmed,
                                            processConfirmProxySk, processProxySecretKey,
                                            runDelegationStateAction)
import           Pos.Delegation.Types      (CheckProxySKConfirmed (..),
                                            CheckProxySKConfirmedRes (..),
                                            ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model             (ListenerDHT (..), MonadDHTDialog, replyToNode)
import           Pos.Types                 (ProxySKEpoch)
import           Pos.WorkMode              (WorkMode)
import           Pos.WorkMode              (NewWorkMode)
import           Node                      (Listener(..), ListenerAction(..), sendTo,
                                            NodeId(..), SendActions(..))
import           Message.Message           (BinaryP, messageName)
import           Mockable.Monad            (MonadMockable(..))
import           Pos.Communication.BiP     (BiP(..))
import           Pos.Ssc.Class.Types       (Ssc(..))
import           Mockable.SharedAtomic     (readSharedAtomic, modifySharedAtomic)

-- | Listeners for requests related to delegation processing.
delegationListeners
    :: ( Ssc ssc
       , NewWorkMode ssc m
       )
    => [ListenerAction BiP m]
delegationListeners =
    [ handleSendProxySK
    , handleConfirmProxySK
    , handleCheckProxySKConfirmed
    ]

----------------------------------------------------------------------------
-- PSKs propagation
----------------------------------------------------------------------------

-- | Handler 'SendProxySK' event.
handleSendProxySK
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => ListenerAction BiP m
handleSendProxySK = ListenerActionOneMsg $
    \_ sendActions (pr :: SendProxySK) -> case pr of
        SendProxySKEpoch pSk -> do
            logDebug "Got request on handleGetHeaders"
            logDebug $ sformat ("Got request to handle proxy secret key: "%build) pSk
            -- do it in worker once in ~sometimes instead of on every request
            curTime <- liftIO getCurrentTime
            runDelegationStateAction $ invalidateProxyCaches curTime
            verdict <- processProxySecretKey pSk
            logResult verdict
            propagateSendProxySK verdict pSk sendActions
          where
            logResult PSKAdded =
                logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
            logResult verdict =
                logDebug $
                sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict     
        _ ->
            logWarning "Heavyweight certificates are not supported yet"

-- | Propagates proxy secret key depending on the decision
propagateSendProxySK
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => PSKVerdict
    -> ProxySKEpoch 
    -> SendActions BiP m
    -> m ()
propagateSendProxySK PSKUnrelated pSk sendActions = do
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating proxy secret key "%build) pSk
        sendProxySecretKey sendActions pSk
propagateSendProxySK PSKAdded pSk sendActions = do
    logDebug $ sformat ("Generating delivery proof and propagating it: "%build) pSk
    sk <- ncSecretKey <$> getNodeContext
    let proof = proxySign sk pSk pSk -- but still proving is nothing but fear
    sendProxyConfirmSK sendActions $ ConfirmProxySK pSk proof
    pure ()
propagateSendProxySK _ _ _ = pure ()

----------------------------------------------------------------------------
-- Light PSKs backpropagation (confirmations)
----------------------------------------------------------------------------

handleConfirmProxySK
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => ListenerAction BiP m
handleConfirmProxySK = ListenerActionOneMsg $
    \_ sendActions ((o@(ConfirmProxySK pSk proof)) :: ConfirmProxySK) -> do
        logDebug $ sformat ("Got request to handle confirmation for psk: "%build) pSk
        verdict <- processConfirmProxySk pSk proof
        propagateConfirmProxySK verdict o sendActions

propagateConfirmProxySK
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => ConfirmPSKVerdict
    -> ConfirmProxySK 
    -> SendActions BiP m
    -> m ()
propagateConfirmProxySK ConfirmPSKValid
                        confPSK@(ConfirmProxySK pSk _)
                        sendActions = do
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating psk confirmation for psk: "%build) pSk        
        sendProxyConfirmSK sendActions confPSK
propagateConfirmProxySK _ _ _ = pure ()

handleCheckProxySKConfirmed
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => ListenerAction BiP m
handleCheckProxySKConfirmed = ListenerActionOneMsg $
    \peerId sendActions (CheckProxySKConfirmed pSk :: CheckProxySKConfirmed) -> do
        logDebug $ sformat ("Got request to check if psk: "%build%" was delivered.") pSk
        res <- runDelegationStateAction $ isProxySKConfirmed pSk
        sendTo sendActions peerId $ CheckProxySKConfirmedRes res
