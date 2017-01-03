{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext (..)

       , putBlkSemaphore
       , readBlkSemaphore
       , takeBlkSemaphore

       , withProxyCaches
       , invalidateProxyCaches

       , readLeaders
       , readRichmen
       , tryReadLeaders
       , putLeaders
       , putRichmen
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Control.Exception       (SomeException)
import           Control.Lens            ((%~))
import           Control.Monad.Catch     (catch)
import qualified Data.HashMap.Strict     as HM
import           Data.Time.Clock         (addUTCTime, getCurrentTime)
import           Universum

import           Pos.Context.Context     (NodeContext (..), ProxyCaches, ncProxyCaches,
                                          ncProxyConfCache, ncProxyMsgCache)
import           Pos.DHT.Model           (DHTResponseT)
import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Types               (HeaderHash, Richmen, SlotLeaders)

-- | Class for something that has 'NodeContext' inside.
class WithNodeContext ssc m | m -> ssc where
    getNodeContext :: m (NodeContext ssc)

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (KademliaDHT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (DHTResponseT s m) where
    getNodeContext = lift getNodeContext


-- TODO Refactor it out of this module when dealing with in-memory
-- things

----------------------------------------------------------------------------
-- Semaphore-related logic
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m (HeaderHash ssc)
takeBlkSemaphore = liftIO . takeMVar . ncBlkSemaphore =<< getNodeContext

putBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => HeaderHash ssc -> m ()
putBlkSemaphore tip = liftIO . flip putMVar tip . ncBlkSemaphore =<< getNodeContext

readBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m (HeaderHash ssc)
readBlkSemaphore = liftIO . readMVar . ncBlkSemaphore =<< getNodeContext

----------------------------------------------------------------------------
-- ProxyCache logic
----------------------------------------------------------------------------

-- | Effectively takes a lock on ProxyCaches mvar in NodeContext and
-- allows you to run some computation producing updated ProxyCaches
-- and return value. Will put MVar back on exception.
withProxyCaches
    :: (MonadIO m, WithNodeContext ssc m, MonadCatch m)
    => (ProxyCaches -> m (a, ProxyCaches)) -> m a
withProxyCaches action = do
    v <- ncProxyCaches <$> getNodeContext
    x <- liftIO $ takeMVar v
    (res,modified) <-
        action x `catch` (\(e :: SomeException) -> liftIO (putMVar v x) >> throwM e)
    liftIO $ putMVar v modified
    pure res

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: (MonadIO m, WithNodeContext ssc m, MonadCatch m) => m ()
invalidateProxyCaches = withProxyCaches $ \p -> do
    curTime <- liftIO $ getCurrentTime
    pure $ ((),) $
        p & ncProxyMsgCache %~ HM.filter (\t -> addUTCTime 60 t > curTime)
          & ncProxyConfCache %~ HM.filter (\t -> addUTCTime 500 t > curTime)

----------------------------------------------------------------------------
-- LRC data
----------------------------------------------------------------------------

-- | Read richmen from node context. This function blocks if
-- participants are not available.
readRichmen
    :: (MonadIO m, WithNodeContext ssc m)
    => m Richmen
readRichmen = getNodeContext >>= liftIO . readMVar . ncSscRichmen

-- | Read slot leaders from node context. This function blocks if
-- leaders are not available.
readLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => m SlotLeaders
readLeaders = getNodeContext >>= liftIO . readMVar . ncSscLeaders

-- | Read slot leaders from node context. Nothing is returned if
-- leaders are not available.
tryReadLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => m (Maybe SlotLeaders)
tryReadLeaders = getNodeContext >>= liftIO . tryReadMVar . ncSscLeaders

-- | Put richmen into MVar, assuming it's empty.
putRichmen
    :: (MonadIO m, WithNodeContext ssc m)
    => Richmen -> m ()
putRichmen richmen = getNodeContext >>= liftIO . flip putMVar richmen . ncSscRichmen

-- | Put leaders into MVar, assuming it's empty.
putLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => SlotLeaders -> m ()
putLeaders leaders = getNodeContext >>= liftIO . flip putMVar leaders . ncSscLeaders
