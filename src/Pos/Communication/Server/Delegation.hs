{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}

-- | Certificate (proxy secret key) propagation listeners and
-- handlers. Small by design. Maybe it makes sense to rename it into
-- "other listeners" and put another stuff here.

module Pos.Communication.Server.Delegation
       ( delegationListeners

       , handleProxySecretKey
       ) where

import           Control.Concurrent.MVar   (putMVar)
import qualified Data.HashMap.Strict       as HM
import           Data.List                 (nub)
import           Data.Time.Clock           (NominalDiffTime, addUTCTime, getCurrentTime)
import           Formatting                (build, sformat, shown, (%))
import           System.Wlog               (logDebug, logInfo)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Communication.Methods (sendProxySecretKey)
import           Pos.Communication.Types   (MutSocketState, ResponseMode,
                                            SendProxySK (..))
import           Pos.Context               (ProxyStorage (..), getNodeContext,
                                            ncPropagation, ncProxyStorage, ncSecretKey)
import           Pos.Crypto                (ProxySecretKey, checkProxySecretKey)
import           Pos.DHT.Model             (ListenerDHT (..), MonadDHTDialog)
import           Pos.Types                 (EpochIndex)
import           Pos.WorkMode              (WorkMode)

-- | Listeners for requests related to blocks processing.
delegationListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
delegationListeners = [ ListenerDHT handleProxySecretKey ]

-- should be a constant
cacheInvalidateTimeout :: NominalDiffTime
cacheInvalidateTimeout = 30 -- sec

withProxyStorage :: (WorkMode ssc m) => (ProxyStorage -> m (a,ProxyStorage)) -> m a
withProxyStorage action = do
    v <- ncProxyStorage <$> getNodeContext
    x <- liftIO $ takeMVar v
    (res,modified) <- action x
    liftIO $ putMVar v modified
    pure res

invalidateCaches :: (WorkMode ssc m) => m ()
invalidateCaches = withProxyStorage $ \ProxyStorage{..} -> do
    curTime <- liftIO $ getCurrentTime
    pure $ ((), ProxyStorage
                { ncProxyCache = HM.filter (\t -> addDelta t > curTime) ncProxyCache
                , .. })
  where
    addDelta = addUTCTime cacheInvalidateTimeout

-- | PSK check verdict. It can be unrelated (other key or spoiled, no
-- way to differ), exist in storage already or be cached.
data PSKVerdict
    = PSKUnrelated
    | PSKExists
    | PSKCached
    | PSKAdded
    deriving Show

-- | Processes proxy secret key (understands do we need it,
-- adds/caches on decision, returns this decision).
processProxySecretKey
    :: (WorkMode ssc m)
    => ProxySecretKey (EpochIndex, EpochIndex) -> m PSKVerdict
processProxySecretKey pSk = withProxyStorage $ \p@ProxyStorage{..} -> do
    sk <- ncSecretKey <$> getNodeContext
    let related = checkProxySecretKey sk pSk
        exists = pSk `elem` ncProxySecretKeys
        cached = HM.member pSk ncProxyCache
    if | cached -> pure (PSKCached, p)
       | exists -> pure (PSKExists, p) -- cache here too?
       | not (related) -> (PSKUnrelated,) <$> cachePSK p
       | otherwise -> pure (PSKAdded, addPSK p)
  where
    addPSK ProxyStorage{..} =
        ProxyStorage { ncProxySecretKeys = nub $ pSk : ncProxySecretKeys, .. }
    cachePSK ProxyStorage{..} = do
        curTime <- liftIO $ getCurrentTime
        pure $ ProxyStorage { ncProxyCache = HM.insert pSk curTime ncProxyCache, .. }

-- | Handler 'SendBlock' event.
handleProxySecretKey
    :: forall ssc m.
       (ResponseMode ssc m)
    => SendProxySK -> m ()
handleProxySecretKey (SendProxySK pSk) = do
    logDebug $ sformat ("Got request to handle proxy secret key: "%build) pSk
    invalidateCaches -- do it in worker once in ~sometimes
    verdict <- processProxySecretKey pSk
    logResult verdict
    propagateProxySecretKey verdict pSk
  where
    logResult PSKAdded =
        logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
    logResult verdict =
        logDebug $
        sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict

-- | Propagates proxy secret key depending on the decision
propagateProxySecretKey
    :: (WorkMode ssc m)
    => PSKVerdict -> ProxySecretKey (EpochIndex, EpochIndex) -> m ()
propagateProxySecretKey PSKUnrelated pSk = do
    whenM (ncPropagation <$> getNodeContext) $ do
        logDebug $ sformat ("Propagating proxy secret key "%build) pSk
        sendProxySecretKey pSk
propagateProxySecretKey _ _ = pure ()
