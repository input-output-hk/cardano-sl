{-|
Module:      Pos.Util.Concurrent.PriorityLock
Description: Provides a prioritised lock

Provides a lock that can be taken with either high or low precedence.
Within each precedence, the lock is taken in FIFO order.
-}

module Pos.Util.Concurrent.PriorityLock
       ( PriorityLock
       , Priority (..)
       , newPriorityLock
       , withPriorityLock
       ) where

import           Control.Concurrent.STM (TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import           Universum

import           Pos.Util.Queue (Q, dequeue, enqueue, queue)

newtype PriorityLock = PriorityLock (TVar PriorityLockState)

data PriorityLockState
    = Unlocked
    | Locked (Q (TMVar ())) (Q (TMVar ()))
    -- ^ locked, with a queue of contenders with high precedence, and
    -- a second queue with contenders of low precedence

data Priority = HighPriority
              | LowPriority

newPriorityLock :: MonadIO m => m PriorityLock
newPriorityLock = liftIO $ PriorityLock <$> newTVarIO Unlocked

lockP :: MonadIO m => PriorityLock -> Priority -> m ()
lockP (PriorityLock vstate) prio = do
    mbwait <- atomically $ do
        readTVar vstate >>= \case
            Unlocked -> do
                -- uncontended, acquire lock, no one is waiting on the lock
                writeTVar vstate (Locked (queue []) (queue []))
                return Nothing

            Locked hwaiters lwaiters -> do
                -- contended, put ourselves on the appropriate queue
                waitvar <- newEmptyTMVar
                case prio of
                    HighPriority ->
                        writeTVar vstate $ Locked (enqueue hwaiters waitvar) lwaiters
                    LowPriority ->
                        writeTVar vstate $ Locked hwaiters (enqueue lwaiters waitvar)
                return (Just waitvar)

    case mbwait of
        Nothing ->
            -- the lock was uncontended, we hold it now
            return ()
        Just waitvar ->
            -- lock was contended, so we have to wait
            atomically $ takeTMVar waitvar
            -- we hold it now

unlockP :: MonadIO m => PriorityLock -> m ()
unlockP (PriorityLock vstate) =
    atomically $ readTVar vstate >>= \case
        Unlocked -> error "Pos.Util.PriorityLock.unlockP: lock is already unlocked"

        Locked hwaiters lwaiters

            -- dequeue from the high priority waiters if possible
            | Just (waiter, hwaiters') <- dequeue hwaiters -> do
                  writeTVar vstate (Locked hwaiters' lwaiters)
                  putTMVar waiter ()

            -- dequeue from the low priority waiters
            | Just (waiter, lwaiters') <- dequeue lwaiters -> do
                  writeTVar vstate (Locked hwaiters lwaiters')
                  putTMVar waiter ()

            -- no one is waiting on the lock, so it will be unlocked now
            | otherwise ->
                  writeTVar vstate Unlocked

withPriorityLock
    :: (MonadMask m, MonadIO m)
    => PriorityLock -> Priority -> m a -> m a
withPriorityLock l prio = bracket_ (lockP l prio) (unlockP l)
