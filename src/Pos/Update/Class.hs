{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class necessary for Update System.

module Pos.Update.Class
       ( MonadUS (..)
       ) where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Except   (ExceptT)
import           Control.Monad.Trans    (MonadTrans)
import           Universum

import           Pos.DHT.Model.Class    (DHTResponseT)
import           Pos.DHT.Real           (KademliaDHT)
import           Pos.Update.MemState    (MemState)

-- | Equivalent of @MonadReader (TVar MemState) m@.
-- TODO: askUSMemState and all the other things should probably be separated
class Monad m => MonadUS m where
    askUSMemState :: m (TVar MemState)
    -- ^ Retrieve 'TVar' on 'Pos.Update.State.MemState'.

    -- | Default implementations for 'MonadTrans'.
    default askUSMemState
        :: (MonadTrans t, MonadUS m', t m' ~ m) => m (TVar MemState)
    askUSMemState = lift askUSMemState

instance MonadUS m => MonadUS (ReaderT s m)
instance MonadUS m => MonadUS (StateT s m)
instance MonadUS m => MonadUS (ExceptT s m)
instance MonadUS m => MonadUS (DHTResponseT s m)
instance MonadUS m => MonadUS (KademliaDHT m)
