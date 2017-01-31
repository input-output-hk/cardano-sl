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
import           Pos.Communication.Protocol (ListenerSpec, OutSpecs, SendActions (..),
                                             listenerOneMsg, mergeLs, oneMsgH, toOutSpecs)
import           Pos.Context                (getNodeContext, ncBlkSemaphore,
                                             ncPropagation)
import           Pos.Delegation.Logic       (ConfirmPskEpochVerdict (..),
                                             PskEpochVerdict (..), PskSimpleVerdict (..),
                                             isProxySKConfirmed, processConfirmProxySk,
                                             processProxySKEpoch, processProxySKSimple,
                                             runDelegationStateAction)
import           Pos.Delegation.Methods     (sendProxyConfirmSK, sendProxyConfirmSKOuts,
                                             sendProxySKEpoch, sendProxySKEpochOuts,
                                             sendProxySKSimple, sendProxySKSimpleOuts)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model              (sendToNeighbors)
import           Pos.Types                  (ProxySKEpoch)
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
    handleDo sendActions req@(SendProxySKSimple pSk) = do
        logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
        verdict <- processProxySKSimple pSk
        logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
        doPropagate <- ncPropagation <$> getNodeContext
        if | verdict == PSIncoherent -> do
               -- We're probably updating state over epoch, so leaders
               -- can be calculated incorrectly.
               blkSemaphore <- ncBlkSemaphore <$> getNodeContext
               void $ liftIO $ readMVar blkSemaphore
               handleDo sendActions req
           | verdict == PSAdded && doPropagate -> do
               logDebug $ sformat ("Propagating heavyweight PSK: "%build) pSk
               sendProxySKSimple pSk sendActions
           | otherwise -> pass
    handleDo sendActions (SendProxySKEpoch pSk) = do
        logDebug "Got request on handleGetHeaders"
        logDebug $ sformat ("Got request to handle lightweight psk: "%build) pSk
        -- do it in worker once in ~sometimes instead of on every request
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

    outSpecs = mconcat [ sendProxySKSimpleOuts
                       , sendProxySKEpochOuts
                       , sendProxyConfirmSKOuts
                       ]

    -- | Propagates lightweight PSK depending on the 'ProxyEpochVerdict'.
    propagateProxySKEpoch
      :: (WorkMode ssc m)
      => PskEpochVerdict -> ProxySKEpoch -> SendActions m -> m ()
    propagateProxySKEpoch PEUnrelated pSk sendActions =
        whenM (ncPropagation <$> getNodeContext) $ do
            logDebug $ sformat ("Propagating lightweight PSK: "%build) pSk
            sendProxySKEpoch pSk sendActions
    propagateProxySKEpoch PEAdded pSk sendActions = sendProxyConfirmSK pSk sendActions
    propagateProxySKEpoch _ _ _ = pass

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
        => ConfirmPskEpochVerdict
        -> ConfirmProxySK
        -> SendActions m1
        -> m1 ()
    propagateConfirmProxySK CPValid
                            confPSK@(ConfirmProxySK pSk _)
                            sendActions = do
        whenM (ncPropagation <$> getNodeContext) $ do
            logDebug $ sformat ("Propagating psk confirmation for psk: "%build) pSk
            sendToNeighbors sendActions confPSK
    propagateConfirmProxySK _ _ _ = pure ()
--
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
