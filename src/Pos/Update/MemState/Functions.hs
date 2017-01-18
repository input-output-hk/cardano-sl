-- | Functions which work in MonadUSMem.

module Pos.Update.MemState.Functions
       ( withUSLock
       ) where

import qualified Control.Concurrent.Lock      as Lock
import           Control.Monad.Catch          (MonadMask, bracket_)
import           Universum

import           Pos.Update.MemState.Class    (MonadUSMem (askUSMemVar))
import           Pos.Update.MemState.MemState (MemVar (..))

withUSLock
    :: (MonadUSMem m, MonadIO m, MonadMask m)
    => m a -> m a
withUSLock action = do
    lock <- mvLock <$> askUSMemVar
    bracket_ (liftIO $ Lock.acquire lock) (liftIO $ Lock.release lock) action
