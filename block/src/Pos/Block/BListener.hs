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
    onRollbackBlocks :: NetworkMagic -> NewestFirst NE Blund -> m SomeBatchOp

instance {-# OVERLAPPABLE #-}
    ( MonadBListener m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadBListener (t m)
  where
    onApplyBlocks nm = lift . onApplyBlocks nm
    onRollbackBlocks nm = lift . onRollbackBlocks nm

onApplyBlocksStub
    :: Monad m
    => NetworkMagic -> OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocksStub _ _ = pure mempty

onRollbackBlocksStub
    :: Monad m
    => NetworkMagic -> NewestFirst NE Blund -> m SomeBatchOp
onRollbackBlocksStub _ _ = pure mempty
