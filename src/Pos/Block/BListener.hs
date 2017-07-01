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

import           Control.Monad.Trans   (MonadTrans (..))
import           Mockable              (SharedAtomicT)

import           Pos.Block.Types       (Blund)
import           Pos.DB.BatchOp        (SomeBatchOp)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Util.Chrono       (NE, NewestFirst (..), OldestFirst (..))


class Monad m => MonadBListener m where
    -- Callback will be called after putting blocks into BlocksDB
    -- and before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onApplyBlocks
        :: forall ssc . SscHelpersClass ssc
        => OldestFirst NE (Blund ssc) -> m SomeBatchOp
    -- Callback will be called before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onRollbackBlocks
        :: forall ssc . SscHelpersClass ssc
        => NewestFirst NE (Blund ssc) -> m SomeBatchOp

instance {-# OVERLAPPABLE #-}
    ( MonadBListener m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadBListener (t m)
  where
    onApplyBlocks = lift . onApplyBlocks
    onRollbackBlocks = lift . onRollbackBlocks

onApplyBlocksStub
    :: forall ssc m . (SscHelpersClass ssc, Monad m)
    => OldestFirst NE (Blund ssc) -> m SomeBatchOp
onApplyBlocksStub _ = pure mempty

onRollbackBlocksStub
    :: forall ssc m . (SscHelpersClass ssc, Monad m)
    => NewestFirst NE (Blund ssc) -> m SomeBatchOp
onRollbackBlocksStub _ = pure mempty
