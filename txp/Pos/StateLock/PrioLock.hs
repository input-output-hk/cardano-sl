{-|
Module:      Pos.StateLock.PrioLock
Description: Provides a prioritised lock

Provides a lock that can be taken with either high or low precedence.
Within each precedence, the lock is taken in FIFO order.
-}

{-# LANGUAGE LambdaCase #-}

module Pos.StateLock.PrioLock where

import           Control.Concurrent.STM      (TMVar, newEmptyTMVar
                                             , putTMVar, takeTMVar)
import           Control.Monad.Catch         (MonadMask)
import           Universum

newtype PrioLock = PrioLock (TVar PrioLockState)

data PrioLockState =
    PUnlocked
    | PLocked [TMVar ()] [TMVar ()] -- TODO: consider using queues instead of lists
    -- ^ locked, with a list of contenders with high precedence, and a
    -- second list with contenders of low precedence

data Priority = High | Low

newPrioLock :: MonadIO m => m PrioLock
newPrioLock = liftIO $ PrioLock <$> newTVarIO PUnlocked

lockP :: MonadIO m => PrioLock -> Priority -> m ()
lockP (PrioLock vstate) prio = do
    mbwait <- atomically $ do
        readTVar vstate >>= \case
            PUnlocked -> do
                -- uncontended, acquire lock, no one is waiting on the lock
                writeTVar vstate (PLocked [] [])
                return Nothing

            PLocked hwaiters lwaiters -> do
                -- contended, put ourselves on the appropriate queue
                waitvar <- newEmptyTMVar
                case prio of
                    High -> writeTVar vstate $ PLocked (hwaiters ++ [waitvar]) lwaiters
                    Low -> writeTVar vstate $ PLocked hwaiters (lwaiters ++ [waitvar])
                return (Just waitvar)

    case mbwait of
        Nothing ->
            -- the lock was uncontended, we hold it now
            return ()
        Just waitvar ->
            -- lock was contended, so we have to wait
            atomically $ takeTMVar waitvar
            -- we hold it now

unlockP :: MonadIO m => PrioLock -> m ()
unlockP (PrioLock vstate) =
    atomically $ readTVar vstate >>= \case
        PUnlocked -> error "Pos.Util.PrioLock.unlockP: lock is already unlocked"

        PLocked [] [] ->
            -- no one is waiting on the lock, so it will be unlocked now
            writeTVar vstate PUnlocked

        PLocked (waiter:hwaiters) lwaiters -> do
            writeTVar vstate (PLocked hwaiters lwaiters)
            putTMVar waiter ()

        PLocked [] (waiter:lwaiters) -> do
            writeTVar vstate (PLocked [] lwaiters)
            putTMVar waiter ()

withPrioLock
    :: (MonadMask m, MonadIO m)
    => PrioLock -> Priority -> m a -> m a
withPrioLock l prio = bracket_ (lockP l prio) (unlockP l)
