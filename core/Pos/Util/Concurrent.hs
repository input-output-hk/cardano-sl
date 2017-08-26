-- | Utilities related to concurrency.

module Pos.Util.Concurrent
       ( clearMVar
       , forcePutMVar
       , readMVarConditional
       , readUntilEqualMVar
       , readTVarConditional
       , readUntilEqualTVar

       -- * 'MVar' functions lifted to 'MonadMask'.
       , withMVar
       , modifyMVar_
       , modifyMVar
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask, mask, onException)
import           Control.Monad.STM   (retry)

clearMVar :: MonadIO m => MVar a -> m ()
clearMVar = void . tryTakeMVar

forcePutMVar :: MonadIO m => MVar a -> a -> m ()
forcePutMVar mvar val = do
    unlessM (tryPutMVar mvar val) $ do
        _ <- tryTakeMVar mvar
        forcePutMVar mvar val

-- | Block until value in MVar satisfies given predicate. When value
-- satisfies, it is returned.
readMVarConditional :: (MonadIO m) => (x -> Bool) -> MVar x -> m x
readMVarConditional predicate mvar = do
    rData <- readMVar mvar -- first we try to read for optimization only
    if predicate rData then pure rData
    else do
        tData <- takeMVar mvar         -- now take data
        if predicate tData then do     -- check again
            _ <- tryPutMVar mvar tData -- try to put taken value
            pure tData
        else
            readMVarConditional predicate mvar

-- | Read until value is equal to stored value comparing by some function.
readUntilEqualMVar
    :: (Eq a, MonadIO m)
    => (x -> a) -> MVar x -> a -> m x
readUntilEqualMVar f mvar expVal = readMVarConditional ((expVal ==) . f) mvar

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
