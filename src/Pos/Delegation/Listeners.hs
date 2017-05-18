{-# LANGUAGE ScopedTypeVariables #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationListeners
       , delegationStubListeners
       ) where

import           Universum

import qualified Ether
import           Formatting                 (build, sformat, shown, (%))
import           System.Wlog                (logDebug, logInfo)


import           Pos.Binary.Communication   ()
import           Pos.Communication.Protocol (ConversationActions (..), ListenerSpec,
                                             OutSpecs, listenerConv)
--import           Pos.Crypto                 (SignTag (SignProxySK), proxySign)
import           Pos.Context                (BlkSemaphore (..), NodeParams, npPropagation)
import           Pos.Delegation.Logic       (ConfirmPskLightVerdict (..),
                                             PskHeavyVerdict (..), PskLightVerdict (..),
                                             processConfirmProxySk, processProxySKHeavy,
                                             processProxySKLight)
import           Pos.Delegation.Methods     (sendProxyConfirmSK, sendProxyConfirmSKOuts,
                                             sendProxySKHeavy, sendProxySKLight,
                                             sendProxySKOuts)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.Types                  (ProxySKHeavy, ProxySKLight)
import           Pos.WorkMode.Class         (WorkMode)

-- | Listeners for requests related to delegation processing.
delegationListeners
--    :: WorkMode ssc m
--    => ([ListenerSpec m], OutSpecs)
    :: [a]
delegationListeners = [] -- mergeLs
    -- [ handleSendProxySK
    -- , handleConfirmProxySK
    -- --, handleCheckProxySKConfirmed
    -- ]

delegationStubListeners :: [a]
delegationStubListeners = []

----------------------------------------------------------------------------
-- PSKs propagation
----------------------------------------------------------------------------

-- | Handler 'SendProxySK' event.
handleSendProxySK
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleSendProxySK =
    listenerConv $ \_ __peerId -> handleDo'
  where
    handleDo' (conv :: ConversationActions Void SendProxySK m) = do
        mReq <- recv conv
        whenJust mReq $ \req -> handleDo req >> handleDo' conv
    handleDo req@(SendProxySKHeavy pSk) = do
        logDebug $ sformat ("GLightLightot request to handle heavyweight psk: "%build) pSk
        verdict <- processProxySKHeavy @ssc pSk
        logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
        -- propagateProxySKHeavy verdict pSk sendActions
    handleDo (SendProxySKLight pSk) = do
        logDebug "Got request on handleGetHeaders"
        logDebug $ sformat ("Got request to handle lightweight psk: "%build) pSk
        -- do it in worker once in ~sometimes instead of on every request
        verdict <- processProxySKLight pSk
        logResult verdict
        -- propagateProxySKLight verdict pSk sendActions
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

    outSpecs = mconcat [ sendProxySKOuts
                       , sendProxyConfirmSKOuts
                       ]
    -- propagateProxySKHeavy
    --   :: (WorkMode ssc m)
    --   => PskHeavyVerdict -> ProxySKHeavy -> SendActions m -> m ()
    -- propagateProxySKHeavy PHIncoherent pSk sendActions = do
    --     -- We're probably updating state over epoch, so leaders
    --     -- can be calculated incorrectly.
    --     blkSemaphore <- Ether.asks' unBlkSemaphore
    --     void $ readMVar blkSemaphore
    --     handleDo sendActions (SendProxySKHeavy pSk)
    -- propagateProxySKHeavy PHAdded pSk sendActions =
    --     whenM (npPropagation <$> Ether.ask @NodeParams) $ do
    --            logDebug $ sformat ("Propagating heavyweight PSK: "%build) pSk
    --            sendProxySKHeavy pSk sendActions
    -- propagateProxySKHeavy _ _ _ = pass

    -- -- | Propagates lightweight PSK depending on the 'PskLightVerdict'.
    -- propagateProxySKLight
    --   :: (WorkMode ssc m)
    --   => PskLightVerdict -> ProxySKLight -> SendActions m -> m ()
    -- propagateProxySKLight PLUnrelated pSk sendActions =
    --     whenM (npPropagation <$> Ether.ask @NodeParams) $ do
    --         logDebug $ sformat ("Propagating lightweight PSK: "%build) pSk
    --         sendProxySKLight pSk sendActions
    -- propagateProxySKLight PLAdded pSk sendActions = do
    --     logDebug $
    --         sformat ("Generating delivery proof and propagating it to neighbors: "%build) psk
    --     sk <- npSecretKey <$> Ether.ask @NodeParams
    --     let proof = proxySign SignProxySK sk pSk pSk -- but still proving is
    --                                                  -- nothing but fear
    --     sendProxyConfirmSK pSk proof sendActions
    -- propagateProxySKLight _ _ _ = pass

----------------------------------------------------------------------------
-- Light PSKs backpropagation (confirmations)
----------------------------------------------------------------------------

handleConfirmProxySK
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleConfirmProxySK =
    listenerConv $ \_ __peerId -> handleDo'
  where
    handleDo' (conv :: ConversationActions Void ConfirmProxySK m) = do
        mReq <- recv conv
        whenJust mReq $ \req -> handleDo req >> handleDo' conv
    handleDo (ConfirmProxySK pSk proof) = do
        logDebug $ sformat ("Got request to handle confirmation for psk: "%build) pSk
        -- verdict <- processConfirmProxySk pSk proof
        -- propagateConfirmProxySK verdict o sendActions

    -- propagateConfirmProxySK
    --     :: ConfirmPskLightVerdict
    --     -> ConfirmProxySK
    --     -> SendActions m
    --     -> m ()
    -- propagateConfirmProxySK CPValid
    --                         confPSK@(ConfirmProxySK pSk _)
    --                         sendActions = do
    --     whenM (npPropagation <$> Ether.ask @NodeParams) $ do
    --         logDebug $ sformat ("Propagating psk confirmation for psk: "%build) pSk
    --         sendProxyConfirmSK pSk proof sendActions
    -- propagateConfirmProxySK _ _ _ = pass

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
