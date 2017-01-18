{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class necessary for Update System.

module Pos.Update.MemState.Class
       ( MonadUSMem (..)
       ) where

import           Control.Monad.Except         (ExceptT)
import           Control.Monad.Trans          (MonadTrans)
import           Universum

import           Pos.Update.MemState.MemState (MemVar)

-- | Reduced equivalent of @MonadReader MemVar m@.
class Monad m => MonadUSMem m where
    askUSMemVar :: m MemVar
    -- ^ Retrieve 'MemVar'.

    -- | Default implementation for 'MonadTrans'.
    default askUSMemVar
        :: (MonadTrans t, MonadUSMem m', t m' ~ m) => m MemVar
    askUSMemVar = lift askUSMemVar

instance MonadUSMem m => MonadUSMem (ReaderT s m)
instance MonadUSMem m => MonadUSMem (StateT s m)
instance MonadUSMem m => MonadUSMem (ExceptT s m)
