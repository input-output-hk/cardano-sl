{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Blockchain listener.
-- Callbacks on application and rollback.

module Pos.Block.BListener
       ( MonadBListener (..)
       ) where

import           Control.Monad.Trans   (MonadTrans (..))
import           Mockable              (SharedAtomicT)
import           Universum

import           Pos.Block.Types       (Blund)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Util.Chrono       (NE, NewestFirst (..), OldestFirst (..))

class Monad m => MonadBListener m where
    -- Callback will be called after putting blocks into BlocksDB
    -- and before changing of GStateDB.
    -- Callback action will be performed under block lock.
    onApplyBlocks
        :: forall ssc . SscHelpersClass ssc
        => OldestFirst NE (Blund ssc) -> m ()
    -- Callback will be called before changing of UtxoDB.
    -- Callback action will be performed under block lock.
    onRollbackBlocks
        :: forall ssc . SscHelpersClass ssc
        => NewestFirst NE (Blund ssc) -> m ()

instance {-# OVERLAPPABLE #-}
    ( MonadBListener m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        MonadBListener (t m)
  where
    onApplyBlocks = lift . onApplyBlocks
    onRollbackBlocks = lift . onRollbackBlocks
