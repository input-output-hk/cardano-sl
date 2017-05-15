{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Blockchain listener.
-- Callbacks on application and rollback.

module Pos.Block.BListener
       ( MonadBListener (..)
       , BListenerStub
       , runBListenerStub
       ) where

import           Control.Monad.Trans          (MonadTrans (..))
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           Mockable                     (SharedAtomicT)
import           Universum

import           Pos.Block.Types              (Blund)
import           Pos.Ssc.Class.Helpers        (SscHelpersClass)
import           Pos.Util.Chrono              (NE, NewestFirst (..), OldestFirst (..))

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


data BListenerStubTag

type BListenerStub = Ether.TaggedTrans BListenerStubTag IdentityT

runBListenerStub :: BListenerStub m a -> m a
runBListenerStub = coerce

-- Blockchain Listener is needed only for Wallet.
-- Stub implementation for usual node.
instance (Monad m) => MonadBListener (BListenerStub m) where
    onApplyBlocks _ = pass
    onRollbackBlocks _ = pass
