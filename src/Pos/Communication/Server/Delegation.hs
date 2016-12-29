{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Certificate (proxy secret key) propagation listeners and
-- handlers. Small by design. Maybe it makes sense to rename it into
-- "other listeners" and put another stuff here.

module Pos.Communication.Server.Delegation
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
import           Pos.Communication.Types   (CheckProxySKConfirmed (..),
                                            CheckProxySKConfirmedRes (..),
                                            ConfirmProxySK (..), MutSocketState,
                                            ResponseMode, SendProxySK (..))
import           Pos.Context               (getNodeContext, invalidateProxyCaches,
                                            ncPropagation, ncProxyConfCache,
                                            ncProxyMsgCache, ncSecretKey, withProxyCaches)
import           Pos.Crypto                (ProxySecretKey, checkProxySecretKey,
                                            pdDelegatePk, pdDelegatePk, proxySign,
                                            proxyVerify)
import           Pos.DB.Misc               (addProxySecretKey, getProxySecretKeys)
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
-- Proxy signing key propagation
----------------------------------------------------------------------------

-- | PSK check verdict. It can be unrelated (other key or spoiled, no
-- way to differ), exist in storage already or be cached.
data PSKVerdict
    = PSKUnrelated
    | PSKExists
    | PSKCached
    | PSKAdded
    deriving (Show)

-- | Processes proxy secret key (understands do we need it,
-- adds/caches on decision, returns this decision).
processProxySecretKey :: (WorkMode ssc m) => ProxySKEpoch -> m PSKVerdict
processProxySecretKey pSk = withProxyCaches $ \p -> do
    sk <- ncSecretKey <$> getNodeContext
    pSks <- getProxySecretKeys
    let related = checkProxySecretKey sk pSk
        exists = pSk `elem` pSks
        cached = p ^. ncProxyMsgCache . to (HM.member pSk)
    if | cached -> pure (PSKCached, p)
       | exists -> pure (PSKExists, p) -- cache here too?
       | not (related) -> (PSKUnrelated,) <$> cachePSK p
       | otherwise -> addProxySecretKey pSk $> (PSKAdded, p)
  where
    cachePSK p = do
        curTime <- liftIO $ getCurrentTime
        pure $ p & ncProxyMsgCache %~ HM.insert pSk curTime

-- | Handler 'SendProxySK' event.
handleSendProxySK
    :: forall ssc m.
       (ResponseMode ssc m)
    => SendProxySK -> m ()
handleSendProxySK (SendProxySK pSk) = do
    logDebug $ sformat ("Got request to handle proxy secret key: "%build) pSk
    invalidateProxyCaches -- do it in worker once in ~sometimes
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
-- PSK confirmation back propagation
----------------------------------------------------------------------------

data ConfirmPSKVerdict
    = ConfirmPSKValid   -- ^ Valid, saved
    | ConfirmPSKInvalid -- ^ Invalid, throw away
    | ConfirmPSKCached  -- ^ Already saved
    deriving (Show)

processConfirmProxySk
    :: (WorkMode ssc m)
    => ProxySKEpoch
    -> ProxySigEpoch ProxySKEpoch
    -> m ConfirmPSKVerdict
processConfirmProxySk pSk proof =
    withProxyCaches $ \p -> do
        let valid = proxyVerify (pdDelegatePk proof) proof (const True) pSk
            cached = p ^. ncProxyConfCache . to (HM.member pSk)
        if | cached -> pure (ConfirmPSKCached, p)
           | not valid -> pure (ConfirmPSKInvalid, p)
           | otherwise -> (ConfirmPSKValid,) <$> cachePsk p
  where
    cachePsk p = do
        curTime <- liftIO $ getCurrentTime
        pure $ p & ncProxyConfCache %~ HM.insert pSk curTime

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

----------------------------------------------------------------------------
-- Check confirmation requests
----------------------------------------------------------------------------

handleCheckProxySKConfirmed
    :: forall ssc m.
       (ResponseMode ssc m)
    => CheckProxySKConfirmed -> m ()
handleCheckProxySKConfirmed (CheckProxySKConfirmed pSk) = do
    logDebug $ sformat ("Got request to check if psk: "%build%" was delivered.") pSk
    res <- withProxyCaches $ \p -> pure (p ^. ncProxyConfCache . to (HM.member pSk), p)
    replyToNode $ CheckProxySKConfirmedRes res

-- response listener should be defined ad-hoc where it's used
