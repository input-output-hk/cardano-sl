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
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.DB.BatchOp (SomeBatchOp)
import           Pos.Util.Trace.Named (TraceNamed)

class Monad m => MonadBListener m where
    -- Callback will be called after putting blocks into BlocksDB
    -- and before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onApplyBlocks :: TraceNamed IO -> OldestFirst NE Blund -> m SomeBatchOp
    -- Callback will be called before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onRollbackBlocks :: TraceNamed IO -> NewestFirst NE Blund -> m SomeBatchOp

instance {-# OVERLAPPABLE #-}
    ( MonadBListener m, Monad m, MonadTrans t, Monad (t m)) =>
        MonadBListener (t m)
  where
    onApplyBlocks logTrace bs = lift $ onApplyBlocks logTrace bs
    onRollbackBlocks logTrace bs = lift $ onRollbackBlocks logTrace bs

onApplyBlocksStub
    :: MonadIO m
    => TraceNamed IO -> OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocksStub _ _ = pure mempty

onRollbackBlocksStub
    :: MonadIO m
    => TraceNamed IO -> NewestFirst NE Blund -> m SomeBatchOp
onRollbackBlocksStub _ _ = pure mempty
