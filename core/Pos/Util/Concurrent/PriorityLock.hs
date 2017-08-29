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

import           Control.Concurrent.STM      (TMVar, newEmptyTMVar
                                             , putTMVar, takeTMVar)
import           Control.Monad.Catch         (MonadMask)
import           Universum                   hiding (empty)

import           Pos.Util.Queue              (Q, empty, queue, enqueue, dequeue)

newtype PriorityLock = PriorityLock (TVar PriorityLockState)

data PriorityLockState =
    Unlocked
    | Locked (Q (TMVar ())) (Q (TMVar ()))
    -- ^ locked, with a queue of contenders with high precedence, and
    -- a second queuewith contenders of low precedence

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
            | empty hwaiters && empty lwaiters ->
              -- no one is waiting on the lock, so it will be unlocked now
              writeTVar vstate Unlocked

        Locked hwaiters lwaiters
            | empty hwaiters -> do
                  -- no one is waiting with high priority, so we
                  -- dequeue from the low priority waiters
                  let (waiter, lwaiters') = dequeue' lwaiters
                  writeTVar vstate (Locked hwaiters lwaiters')
                  putTMVar waiter ()

        Locked hwaiters lwaiters -> do
            -- dequeue from the high priority waiters
            let (waiter, hwaiters') = dequeue' hwaiters
            writeTVar vstate (Locked hwaiters' lwaiters)
            putTMVar waiter ()
    where dequeue' q = fromMaybe (error "Logic error in PriorityLock") (dequeue q)

withPriorityLock
    :: (MonadMask m, MonadIO m)
    => PriorityLock -> Priority -> m a -> m a
withPriorityLock l prio = bracket_ (lockP l prio) (unlockP l)
