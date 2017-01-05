{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class necessary for Update System.

module Pos.Update.Class
       ( MonadUS (..)
       ) where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Trans    (MonadTrans)
import           Universum

import           Pos.DHT.Model.Class    (DHTResponseT)
import           Pos.DHT.Real           (KademliaDHT)
import           Pos.Update.MemState    (MemState)

-- | Equivalent of @MonadReader (TVar MemState) m@.
class Monad m => MonadUS m where
    askUSMemState :: m (TVar MemState)
    -- ^ Retrieve 'TVar' on 'Pos.Update.State.MemState'.

    default askUSMemState
        :: (MonadTrans t, MonadUS m', t m' ~ m) => m (TVar MemState)
    askUSMemState = lift askUSMemState
    -- ^ Default implementation for 'MonadTrans'.

instance MonadUS m => MonadUS (ReaderT s m)
instance MonadUS m => MonadUS (StateT s m)
instance MonadUS m => MonadUS (DHTResponseT s m)
instance MonadUS m => MonadUS (KademliaDHT m)
