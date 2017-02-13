{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationListeners
       , delegationStubListeners

       , handleSendProxySK
       , handleConfirmProxySK
       --, handleCheckProxySKConfirmed
       ) where

import           Data.Proxy                 (Proxy (..))
import           Formatting                 (build, sformat, shown, (%))
import           Pos.Communication.Util     (stubListenerOneMsg)
import           System.Wlog                (WithLogger, logDebug, logInfo)
import           Universum


import           Pos.Binary.Communication   ()
import           Pos.Communication.Protocol (ListenerSpec, OutSpecs, SendActions,
                                             listenerOneMsg, mergeLs, oneMsgH, toOutSpecs)
import           Pos.Context                (getNodeContext, ncBlkSemaphore, ncNodeParams,
                                             npPropagation)
import           Pos.Delegation.Logic       (ConfirmPskLightVerdict (..),
                                             PskHeavyVerdict (..), PskLightVerdict (..),
                                             processConfirmProxySk, processProxySKHeavy,
                                             processProxySKLight)
import           Pos.Delegation.Methods     (sendProxyConfirmSK, sendProxyConfirmSKOuts,
                                             sendProxySKHeavy, sendProxySKHeavyOuts,
                                             sendProxySKLight, sendProxySKLightOuts)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model              (sendToNeighbors)
import           Pos.Types                  (ProxySKLight)
import           Pos.WorkMode               (WorkMode)

-- | Listeners for requests related to delegation processing.
delegationListeners
    :: WorkMode ssc m
    => ([ListenerSpec m], OutSpecs)
delegationListeners = mergeLs
    [ handleSendProxySK
    , handleConfirmProxySK
    --, handleCheckProxySKConfirmed
    ]

delegationStubListeners
    :: WithLogger m
    => ([ListenerSpec m], OutSpecs)
delegationStubListeners = mergeLs
    [ stubListenerOneMsg (Proxy :: Proxy SendProxySK)
    , stubListenerOneMsg (Proxy :: Proxy ConfirmProxySK)
    --, stubListenerOneMsg (Proxy :: Proxy CheckProxySKConfirmed)
    ]

----------------------------------------------------------------------------
-- PSKs propagation
----------------------------------------------------------------------------

-- | Handler 'SendProxySK' event.
handleSendProxySK
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleSendProxySK = listenerOneMsg outSpecs $
    \_ _ sendActions (pr :: SendProxySK) -> handleDo sendActions pr
  where
    handleDo sendActions req@(SendProxySKHeavy pSk) = do
        logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
        verdict <- processProxySKHeavy pSk
        logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
        doPropagate <- npPropagation . ncNodeParams <$> getNodeContext
        if | verdict == PHIncoherent -> do
               -- We're probably updating state over epoch, so leaders
               -- can be calculated incorrectly.
               blkSemaphore <- ncBlkSemaphore <$> getNodeContext
               void $ liftIO $ readMVar blkSemaphore
               handleDo sendActions req
           | verdict == PHAdded && doPropagate -> do
               logDebug $ sformat ("Propagating heavyweight PSK: "%build) pSk
               sendProxySKHeavy pSk sendActions
           | otherwise -> pass
    handleDo sendActions (SendProxySKLight pSk) = do
        logDebug "Got request on handleGetHeaders"
        logDebug $ sformat ("Got request to handle lightweight psk: "%build) pSk
        -- do it in worker once in ~sometimes instead of on every request
        verdict <- processProxySKLight pSk
        logResult verdict
        propagateProxySKLight verdict pSk sendActions
      where
        logResult PLAdded =
            logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
        logResult PLRemoved =
            logInfo $
            sformat ("Removing keys from issuer because got "%
                     "self-signed revocation: "%build) pSk
        logResult verdict =
            logDebug $
            sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict

    outSpecs = mconcat [ sendProxySKHeavyOuts
                       , sendProxySKLightOuts
                       , sendProxyConfirmSKOuts
                       ]

    -- | Propagates lightweight PSK depending on the 'PskLightVerdict'.
    propagateProxySKLight
      :: (WorkMode ssc m)
      => PskLightVerdict -> ProxySKLight -> SendActions m -> m ()
    propagateProxySKLight PLUnrelated pSk sendActions =
        whenM (npPropagation . ncNodeParams <$> getNodeContext) $ do
            logDebug $ sformat ("Propagating lightweight PSK: "%build) pSk
            sendProxySKLight pSk sendActions
    propagateProxySKLight PLAdded pSk sendActions = sendProxyConfirmSK pSk sendActions
    propagateProxySKLight _ _ _ = pass

----------------------------------------------------------------------------
-- Light PSKs backpropagation (confirmations)
----------------------------------------------------------------------------

handleConfirmProxySK
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleConfirmProxySK = listenerOneMsg outSpecs $
    \_ _ sendActions ((o@(ConfirmProxySK pSk proof)) :: ConfirmProxySK) -> do
        logDebug $ sformat ("Got request to handle confirmation for psk: "%build) pSk
        verdict <- processConfirmProxySk pSk proof
        propagateConfirmProxySK verdict o sendActions
  where
    outSpecs = toOutSpecs [oneMsgH (Proxy :: Proxy ConfirmProxySK)]

    propagateConfirmProxySK
        :: forall ssc1 m1. (WorkMode ssc1 m1)
        => ConfirmPskLightVerdict
        -> ConfirmProxySK
        -> SendActions m1
        -> m1 ()
    propagateConfirmProxySK CPValid
                            confPSK@(ConfirmProxySK pSk _)
                            sendActions = do
        whenM (npPropagation . ncNodeParams <$> getNodeContext) $ do
            logDebug $ sformat ("Propagating psk confirmation for psk: "%build) pSk
            sendToNeighbors sendActions confPSK
    propagateConfirmProxySK _ _ _ = pure ()

--handleCheckProxySKConfirmed
--    :: forall ssc m.
--       (WorkMode ssc m)
--    => (ListenerSpec m, OutSpecs)
--handleCheckProxySKConfirmed = listenerOneMsg outSpecs $
--    \_ peerId sendActions (CheckProxySKConfirmed pSk :: CheckProxySKConfirmed) -> do
--        logDebug $ sformat ("Got request to check if psk: "%build%" was delivered.") pSk
--        res <- runDelegationStateAction $ isProxySKConfirmed pSk
--        sendTo sendActions peerId $ CheckProxySKConfirmedRes res
--  where
--    outSpecs = toOutSpecs [oneMsgH (Proxy :: Proxy CheckProxySKConfirmedRes)]
