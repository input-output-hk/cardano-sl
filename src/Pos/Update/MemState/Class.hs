{-# LANGUAGE TypeFamilies         #-}

-- | Type class necessary for Update System.

module Pos.Update.MemState.Class
       ( MonadUSMem (..)
       , askUSMemState
       ) where

import           Control.Concurrent.STM    (TVar)
import           Control.Monad.Except      (ExceptT)
import           Control.Monad.Trans       (MonadTrans)
import           Universum

import           Pos.Update.MemState.Types (MemState, MemVar (mvState))

-- | Reduced equivalent of @MonadReader MemVar m@.
class Monad m => MonadUSMem m where
    askUSMemVar :: m MemVar
    -- ^ Retrieve 'MemVar'.

    -- | Default implementation for 'MonadTrans'.
    default askUSMemVar
        :: (MonadTrans t, MonadUSMem m', t m' ~ m) => m MemVar
    askUSMemVar = lift askUSMemVar

askUSMemState :: MonadUSMem m => m (TVar MemState)
askUSMemState = mvState <$> askUSMemVar

instance MonadUSMem m => MonadUSMem (ReaderT s m)
instance MonadUSMem m => MonadUSMem (StateT s m)
instance MonadUSMem m => MonadUSMem (ExceptT s m)
