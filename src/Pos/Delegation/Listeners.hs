{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationListeners
       , delegationStubListeners

       , handleSendProxySK
       , handleConfirmProxySK
       , handleCheckProxySKConfirmed
       ) where

import           Data.Proxy               (Proxy (..))
import           Data.Time.Clock          (getCurrentTime)
import           Formatting               (build, sformat, shown, (%))
import           System.Wlog              (WithLogger, logDebug, logInfo)
import           Universum

import           Node                     (ListenerAction (..), SendActions (..), sendTo)


import           Pos.Binary.Communication ()
import           Pos.Communication.BiP    (BiP (..))
import           Pos.Context              (getNodeContext, ncPropagation)
import           Pos.Delegation.Logic     (ConfirmPskEpochVerdict (..),
                                           PskEpochVerdict (..), PskSimpleVerdict (..),
                                           invalidateProxyCaches, isProxySKConfirmed,
                                           processConfirmProxySk, processProxySKEpoch,
                                           processProxySKSimple, runDelegationStateAction)
import           Pos.Delegation.Methods   (sendProxyConfirmSK, sendProxySKEpoch,
                                           sendProxySKSimple)
import           Pos.Delegation.Types     (CheckProxySKConfirmed (..),
                                           CheckProxySKConfirmedRes (..),
                                           ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model            (sendToNeighbors)
import           Pos.Types                (ProxySKEpoch)
import           Pos.Util                 (stubListenerOneMsg)
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to delegation processing.
delegationListeners
    :: ( WorkMode ssc m
       )
    => [ListenerAction BiP m]
delegationListeners =
    [ handleSendProxySK
    , handleConfirmProxySK
    , handleCheckProxySKConfirmed
    ]

delegationStubListeners
    :: WithLogger m
    => [ListenerAction BiP m]
delegationStubListeners =
    [ stubListenerOneMsg (Proxy :: Proxy SendProxySK)
    , stubListenerOneMsg (Proxy :: Proxy ConfirmProxySK)
    , stubListenerOneMsg (Proxy :: Proxy CheckProxySKConfirmed)
    ]

----------------------------------------------------------------------------
-- PSKs propagation
----------------------------------------------------------------------------

-- | Handler 'SendProxySK' event.
handleSendProxySK
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP m
handleSendProxySK = ListenerActionOneMsg $
    \_ sendActions (pr :: SendProxySK) -> case pr of
        SendProxySKEpoch pSk -> do
            logDebug "Got request on handleGetHeaders"
            logDebug $ sformat ("Got request to handle lightweight psk: "%build) pSk
            -- do it in worker once in ~sometimes instead of on every request
            curTime <- liftIO getCurrentTime
            runDelegationStateAction $ invalidateProxyCaches curTime
            verdict <- processProxySKEpoch pSk
            logResult verdict
            propagateProxySKEpoch verdict pSk sendActions
          where
            logResult PEAdded =
                logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
            logResult PERemoved =
                logInfo $
                sformat ("Removing keys from issuer because got "%
                         "self-signed revocation: "%build) pSk
            logResult verdict =
                logDebug $
                sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict
        SendProxySKSimple pSk -> do
            logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
            verdict <- processProxySKSimple pSk
            logDebug $ "The verdict is: " <> show verdict
            doPropagate <- ncPropagation <$> getNodeContext
            when (verdict == PSAdded && doPropagate) $ do
                logDebug $ sformat ("Propagating heavyweight PSK: "%build) pSk
                sendProxySKSimple sendActions pSk

-- | Propagates lightweight PSK depending on the 'ProxyEpochVerdict'.
propagateProxySKEpoch
  :: (WorkMode ssc m)
  => PskEpochVerdict -> ProxySKEpoch -> SendActions BiP m -> m ()
propagateProxySKEpoch PEUnrelated pSk sendActions =
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating lightweight PSK: "%build) pSk
        sendProxySKEpoch sendActions pSk
propagateProxySKEpoch PEAdded pSk sendActions = sendProxyConfirmSK sendActions pSk
propagateProxySKEpoch _ _ _ = pass

----------------------------------------------------------------------------
-- Light PSKs backpropagation (confirmations)
----------------------------------------------------------------------------

handleConfirmProxySK
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP m
handleConfirmProxySK = ListenerActionOneMsg $
    \_ sendActions ((o@(ConfirmProxySK pSk proof)) :: ConfirmProxySK) -> do
        logDebug $ sformat ("Got request to handle confirmation for psk: "%build) pSk
        verdict <- processConfirmProxySk pSk proof
        propagateConfirmProxySK verdict o sendActions

propagateConfirmProxySK
    :: forall ssc m.
       (WorkMode ssc m)
    => ConfirmPskEpochVerdict
    -> ConfirmProxySK
    -> SendActions BiP m
    -> m ()
propagateConfirmProxySK CPValid
                        confPSK@(ConfirmProxySK pSk _)
                        sendActions = do
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating psk confirmation for psk: "%build) pSk
        sendToNeighbors sendActions confPSK
propagateConfirmProxySK _ _ _ = pure ()

handleCheckProxySKConfirmed
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP m
handleCheckProxySKConfirmed = ListenerActionOneMsg $
    \peerId sendActions (CheckProxySKConfirmed pSk :: CheckProxySKConfirmed) -> do
        logDebug $ sformat ("Got request to check if psk: "%build%" was delivered.") pSk
        res <- runDelegationStateAction $ isProxySKConfirmed pSk
        sendTo sendActions peerId $ CheckProxySKConfirmedRes res
