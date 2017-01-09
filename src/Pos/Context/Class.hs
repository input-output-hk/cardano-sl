{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext (..)

       , putBlkSemaphore
       , readBlkSemaphore
       , takeBlkSemaphore

       , readLeaders
       , readRichmen
       , tryReadLeaders
       , putLeaders
       , putRichmen
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Universum

import           Pos.Context.Context     (NodeContext (..))
import qualified Pos.DHT.Model           as OldDHT
import qualified Pos.DHT.Real            as OldDHT
import           Pos.Types               (HeaderHash, Richmen, SlotLeaders)

-- | Class for something that has 'NodeContext' inside.
class WithNodeContext ssc m | m -> ssc where
    getNodeContext :: m (NodeContext ssc)

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (OldDHT.KademliaDHT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (OldDHT.DHTResponseT s m) where
    getNodeContext = lift getNodeContext


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
