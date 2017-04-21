{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.Delegation.Holder
       ( DelegationT
       , runDelegationT
       , runDelegationTFromTVar
       ) where

import qualified Control.Monad.Ether.Implicit as Ether
import           Pos.Delegation.Class         (DelegationWrap (..))
import           Universum


-- | Wrapper of @ReaderT (TVar DelegationWrap)@, nothing smart.
type DelegationT = Ether.ReaderT (TVar DelegationWrap)

-- | Executes delegationT transformer creating tvar from given wrap.
runDelegationT :: MonadIO m => DelegationWrap -> DelegationT m a -> m a
runDelegationT wrap action =
    liftIO (newTVarIO wrap) >>= Ether.runReaderT action

-- | Executes delegation wrap using existing delegation wrap tvar.
runDelegationTFromTVar :: TVar DelegationWrap -> DelegationT m a -> m a
runDelegationTFromTVar = flip Ether.runReaderT
