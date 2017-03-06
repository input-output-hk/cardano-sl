{-# LANGUAGE TypeFamilies #-}

module Pos.Shutdown.Class
       ( MonadShutdownMem (..)
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Universum

import           Pos.Shutdown.Types  (ShutdownContext)

class Monad m => MonadShutdownMem m where
    askShutdownMem :: m ShutdownContext

    default askShutdownMem :: (MonadTrans t, MonadShutdownMem m', t m' ~ m) =>
       m ShutdownContext
    askShutdownMem = lift askShutdownMem

instance MonadShutdownMem m => MonadShutdownMem (ReaderT s m) where
instance MonadShutdownMem m => MonadShutdownMem (ExceptT s m) where
instance MonadShutdownMem m => MonadShutdownMem (StateT s m) where
