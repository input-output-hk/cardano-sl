{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Blockchain listener.
-- Callbacks on application and rollback.

module Pos.Block.BListener
       ( MonadBListener (..)
       , onApplyBlocksStub
       , onRollbackBlocksStub
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans (..))
import           Mockable (SharedAtomicT)

import           Pos.Block.Types (Blund)
import           Pos.DB.BatchOp (SomeBatchOp)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..))

class Monad m => MonadBListener m where
    -- Callback will be called after putting blocks into BlocksDB
    -- and before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onApplyBlocks :: OldestFirst NE Blund -> m SomeBatchOp
    -- Callback will be called before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onRollbackBlocks :: NewestFirst NE Blund -> m SomeBatchOp

instance {-# OVERLAPPABLE #-}
    ( MonadBListener m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadBListener (t m)
  where
    onApplyBlocks = lift . onApplyBlocks
    onRollbackBlocks = lift . onRollbackBlocks

onApplyBlocksStub
    :: Monad m
    => OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocksStub _ = pure mempty

onRollbackBlocksStub
    :: Monad m
    => NewestFirst NE Blund -> m SomeBatchOp
onRollbackBlocksStub _ = pure mempty
