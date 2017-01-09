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

       , readLeadersEager
       , readLeaders
       , tryReadLeaders
       , tryReadLeadersEpoch
       , writeLeaders
       , isLeadersComputed
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Universum

import           Pos.Context.Class       (WithNodeContext (..))
import           Pos.Context.Context     (NodeContext (..))
import           Pos.Types               (EpochIndex, HeaderHash, SlotLeaders, Utxo)
import           Pos.Util                (forcePutMVar, readUntilEqualMVar)

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
----------------------------------------------------------------------------

-- | Read slot leaders from node context. This function blocks if
-- leaders are not available.
readLeadersEager
    :: (MonadIO m, WithNodeContext ssc m)
    => m (EpochIndex, SlotLeaders)
readLeadersEager = getNodeContext >>= liftIO . readMVar . ncSscLeaders

-- | Read slot leaders from node context. Nothing is returned if
-- leaders are not available.
tryReadLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => m (Maybe (EpochIndex, SlotLeaders))
tryReadLeaders = getNodeContext >>= liftIO . tryReadMVar . ncSscLeaders

-- | Force put leaders into MVar.
writeLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => (EpochIndex, SlotLeaders) -> m ()
writeLeaders el = getNodeContext >>= flip forcePutMVar el . ncSscLeaders

-- Function which using epoch as parameter

-- | Wait until computation epoch equals expEpoch
-- and return after that.
readLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => EpochIndex -> m SlotLeaders
readLeaders expEpoch = do
    nc <- getNodeContext
    snd <$> readUntilEqualMVar fst (ncSscLeaders nc) expEpoch

-- | Return Just if value is present and
-- computation epoch equals expEpoch, Nothing otherwise.
tryReadLeadersEpoch
    :: (MonadIO m, WithNodeContext ssc m)
    => EpochIndex -> m (Maybe SlotLeaders)
tryReadLeadersEpoch expEpoch = do
    dt <- tryReadLeaders
    pure $ maybe Nothing (\(e, l) -> if e == expEpoch then Just l else Nothing) dt

-- | Returns True if leaders are computed for the specified epoch
isLeadersComputed
    :: (MonadIO m, WithNodeContext ssc m)
    => EpochIndex -> m Bool
isLeadersComputed epoch = isJust <$> tryReadLeadersEpoch epoch
