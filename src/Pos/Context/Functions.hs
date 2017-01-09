{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         genesisUtxoM
       , genesisLeadersM

         -- * Block semaphore.
       , putBlkSemaphore
       , readBlkSemaphore
       , takeBlkSemaphore

       -- FIXME: move somewhere
       , waitLrc
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Control.TimeWarp.Timed  (MonadTimed, for, ms, wait)
import           Universum

import           Pos.Context.Class       (WithNodeContext (..))
import           Pos.Context.Context     (NodeContext (..))
import           Pos.Types               (EpochIndex, HeaderHash, SlotLeaders, Utxo)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM :: (Functor m, WithNodeContext ssc m) => m Utxo
genesisUtxoM = ncGenesisUtxo <$> getNodeContext

genesisLeadersM :: (Functor m, WithNodeContext ssc m) => m SlotLeaders
genesisLeadersM = ncGenesisLeaders <$> getNodeContext

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
-- FIXME: move somewhere
----------------------------------------------------------------------------

-- FIXME: this function is very bad, it will be changed soon.
-- | Block until readers are available and then read them.
waitLrc
    :: (MonadTimed m, MonadIO m, WithNodeContext ssc m)
    => (EpochIndex -> m (Maybe a)) -> EpochIndex -> m ()
waitLrc checker epoch = () <$ do
    leaders <- checker epoch
    maybe (shortWait >> waitLrc checker epoch) (void . pure) leaders
  where
    -- FIXME: use MVar or TVar
    shortWait = wait $ for 10 ms
