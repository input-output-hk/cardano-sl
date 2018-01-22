-- | Utilities related to concurrency.

module Pos.Util.Concurrent
       ( clearMVar
       , readTVarConditional
       , readUntilEqualTVar

       -- * 'MVar' functions lifted to 'MonadMask'.
       , withMVar
       , modifyMVar_
       , modifyMVar
       ) where

import           Universum

import           Control.Exception.Safe (onException)
import           Control.Monad.STM (retry)

clearMVar :: MonadIO m => MVar a -> m ()
clearMVar = void . tryTakeMVar

-- | Block until value in TVar satisfies given predicate. When value
-- satisfies, it is returned.
readTVarConditional :: (MonadIO m) => (x -> Bool) -> TVar x -> m x
readTVarConditional predicate tvar = atomically $ do
    res <- readTVar tvar
    if predicate res then pure res
    else retry

-- | Read until value is equal to stored value comparing by some function.
readUntilEqualTVar
    :: (Eq a, MonadIO m)
    => (x -> a) -> TVar x -> a -> m x
readUntilEqualTVar f tvar expVal = readTVarConditional ((expVal ==) . f) tvar

-- | Version of 'withMVar' which uses 'MonadMask' capabilities for
-- exception handling. There seems to be no such function in any
-- library. This definition was copied from 'base'.
withMVar :: (MonadIO m, MonadMask m) => MVar a -> (a -> m b) -> m b
withMVar m io =
    mask $ \restore -> do
        a <- takeMVar m
        b <- restore (io a) `onException` putMVar m a
        putMVar m a
        return b

-- | Version of 'modifyMVar_' which uses 'MonadMask' capabilities for
-- exception handling. There seems to be no such function in any
-- library. This definition was copied from 'base'.
modifyMVar_ :: (MonadIO m, MonadMask m) => MVar a -> (a -> m a) -> m ()
modifyMVar_ m io =
    mask $ \restore -> do
        a <- takeMVar m
        a' <- restore (io a) `onException` putMVar m a
        putMVar m a'

-- | Version of 'modifyMVar' which uses 'MonadMask' capabilities for
-- exception handling. There seems to be no such function in any
-- library. This definition was copied from 'base'.
modifyMVar :: (MonadIO m, MonadMask m) => MVar a -> (a -> m (a,b)) -> m b
modifyMVar m io =
    mask $ \restore -> do
        a <- takeMVar m
        (a', b) <- restore (io a >>= evaluateWHNF) `onException` putMVar m a
        putMVar m a'
        return b
