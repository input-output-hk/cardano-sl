{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Blockchain listener.
-- Callbacks on application and rollback.

module Pos.DB.Block.BListener
       ( MonadBListener (..)
       , onApplyBlocksStub
       , onRollbackBlocksStub
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans (..))

import           Pos.Chain.Block (Blund)
import           Pos.Core (ProtocolConstants)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.DB.BatchOp (SomeBatchOp)

class Monad m => MonadBListener m where
    -- Callback will be called after putting blocks into BlocksDB
    -- and before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onApplyBlocks :: NetworkMagic -> OldestFirst NE Blund -> m SomeBatchOp
    -- Callback will be called before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onRollbackBlocks :: NetworkMagic -> ProtocolConstants -> NewestFirst NE Blund -> m SomeBatchOp

instance {-# OVERLAPPABLE #-}
    ( MonadBListener m, Monad m, MonadTrans t, Monad (t m)) =>
        MonadBListener (t m)
  where
    onApplyBlocks nm = lift . onApplyBlocks nm
    onRollbackBlocks nm pc = lift . onRollbackBlocks nm pc

onApplyBlocksStub
    :: Monad m
    => NetworkMagic -> OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocksStub _ _ = pure mempty

onRollbackBlocksStub
    :: Monad m
    => NetworkMagic -> NewestFirst NE Blund -> m SomeBatchOp
onRollbackBlocksStub _ _ = pure mempty
