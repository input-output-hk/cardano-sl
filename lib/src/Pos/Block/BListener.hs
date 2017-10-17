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
import           Pos.Ssc.GodTossing.Type (SscGodTossing)

class Monad m => MonadBListener m where
    -- Callback will be called after putting blocks into BlocksDB
    -- and before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onApplyBlocks
        :: SscHelpersClass SscGodTossing
        => OldestFirst NE Blund -> m SomeBatchOp
    -- Callback will be called before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onRollbackBlocks
        :: SscHelpersClass SscGodTossing
        => NewestFirst NE Blund -> m SomeBatchOp

instance {-# OVERLAPPABLE #-}
    ( MonadBListener m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadBListener (t m)
  where
    onApplyBlocks = lift . onApplyBlocks
    onRollbackBlocks = lift . onRollbackBlocks

onApplyBlocksStub
    :: (SscHelpersClass SscGodTossing, Monad m)
    => OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocksStub _ = pure mempty

onRollbackBlocksStub
    :: (SscHelpersClass SscGodTossing, Monad m)
    => NewestFirst NE Blund -> m SomeBatchOp
onRollbackBlocksStub _ = pure mempty
