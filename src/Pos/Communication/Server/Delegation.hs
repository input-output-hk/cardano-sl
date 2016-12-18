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
       ) where

import           Control.Concurrent.MVar   (putMVar)
import           Control.Exception         (SomeException)
import           Control.Lens              (to, (%~), (^.))
import           Control.Monad.Catch       (catch)
import qualified Data.HashMap.Strict       as HM
import           Data.List                 (nub)
import           Data.Time.Clock           (NominalDiffTime, addUTCTime, getCurrentTime)
import           Formatting                (build, sformat, shown, (%))
import           System.Wlog               (logDebug, logInfo)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Communication.Methods (sendProxyConfirmSK, sendProxySecretKey)
import           Pos.Communication.Types   (ConfirmProxySK (..), MutSocketState,
                                            ResponseMode, SendProxySK (..))
import           Pos.Context               (ProxyStorage, getNodeContext, ncPropagation,
                                            ncProxyCache, ncProxyConfCache,
                                            ncProxySecretKeys, ncProxyStorage,
                                            ncSecretKey)
import           Pos.Crypto                (ProxySecretKey, ProxySignature,
                                            checkProxySecretKey, pdDelegatePk,
                                            pdDelegatePk, proxyVerify)
import           Pos.DHT.Model             (ListenerDHT (..), MonadDHTDialog)
import           Pos.Types                 (EpochIndex)
import           Pos.WorkMode              (WorkMode)

-- | Listeners for requests related to blocks processing.
delegationListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
delegationListeners =
    [ ListenerDHT handleSendProxySK
    , ListenerDHT handleConfirmProxySK
    ]

----------------------------------------------------------------------------
-- Logic of delegation-storage related stuff
----------------------------------------------------------------------------

withProxyStorage :: (WorkMode ssc m) => (ProxyStorage -> m (a,ProxyStorage)) -> m a
withProxyStorage action = do
    v <- ncProxyStorage <$> getNodeContext
    x <- liftIO $ takeMVar v
    (res,modified) <-
        action x `catch` (\(e :: SomeException) -> liftIO (putMVar v x) >> throwM e)
    liftIO $ putMVar v modified
    pure res

invalidateCaches :: (WorkMode ssc m) => m ()
invalidateCaches = withProxyStorage $ \p -> do
    curTime <- liftIO $ getCurrentTime
    pure $ ((),) $
        p & ncProxyCache %~ HM.filter (\t -> addUTCTime 30 t > curTime)
          & ncProxyConfCache %~ HM.filter (\t -> addUTCTime 500 t > curTime)

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
processProxySecretKey
    :: (WorkMode ssc m)
    => ProxySecretKey (EpochIndex, EpochIndex) -> m PSKVerdict
processProxySecretKey pSk = withProxyStorage $ \p -> do
    sk <- ncSecretKey <$> getNodeContext
    let related = checkProxySecretKey sk pSk
        exists = p ^. ncProxySecretKeys . to (elem pSk)
        cached = p ^. ncProxyCache . to (HM.member pSk)
    if | cached -> pure (PSKCached, p)
       | exists -> pure (PSKExists, p) -- cache here too?
       | not (related) -> (PSKUnrelated,) <$> cachePSK p
       | otherwise -> pure (PSKAdded, addPSK p)
  where
    addPSK p = p & ncProxySecretKeys %~ nub . (pSk:)
    cachePSK p = do
        curTime <- liftIO $ getCurrentTime
        pure $ p & ncProxyCache %~ HM.insert pSk curTime

-- | Handler 'SendProxySK' event.
handleSendProxySK
    :: forall ssc m.
       (ResponseMode ssc m)
    => SendProxySK -> m ()
handleSendProxySK (SendProxySK pSk) = do
    logDebug $ sformat ("Got request to handle proxy secret key: "%build) pSk
    invalidateCaches -- do it in worker once in ~sometimes
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
propagateSendProxySK _ _ = pure ()

----------------------------------------------------------------------------
-- PSK confirmation back propagation
----------------------------------------------------------------------------

data ConfirmPSKVerdict
    = ConfirmPSKValid -- ^ Valid, saved
    | ConfirmPSKInvalid -- ^ Invalid, throw away
    | ConfirmPSKCached -- ^ Already saved

processConfirmProxySk
    :: (WorkMode ssc m)
    => ProxySecretKey (EpochIndex, EpochIndex)
    -> ProxySignature (EpochIndex, EpochIndex)
                      (ProxySecretKey (EpochIndex, EpochIndex))
    -> m ConfirmPSKVerdict
processConfirmProxySk pSk proof =
    withProxyStorage $ \p -> do
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
