{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class necessary for Update System.

module Pos.Update.MemState.Class
       ( MonadUSMem (..)
       ) where

import           Control.Concurrent.STM       (TVar)
import           Control.Monad.Except         (ExceptT)
import           Control.Monad.Trans          (MonadTrans)
import           Universum

import           Pos.Update.MemState.MemState (MemState)

-- | Equivalent of @MonadReader (TVar MemState) m@.
-- TODO: askUSMemState and all the other things should probably be separated
class Monad m => MonadUSMem m where
    askUSMemState :: m (TVar MemState)
    -- ^ Retrieve 'TVar' on 'Pos.Update.State.MemState'.

    -- | Default implementations for 'MonadTrans'.
    default askUSMemState
        :: (MonadTrans t, MonadUSMem m', t m' ~ m) => m (TVar MemState)
    askUSMemState = lift askUSMemState

instance MonadUSMem m => MonadUSMem (ReaderT s m)
instance MonadUSMem m => MonadUSMem (StateT s m)
instance MonadUSMem m => MonadUSMem (ExceptT s m)
