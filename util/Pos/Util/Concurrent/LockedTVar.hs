-- | Locked 'TVar' which can be used to synchronize in-memory data
-- with outside world.

module Pos.Util.Concurrent.LockedTVar
       ( LockedTVar
       , newLockedTVar
       , writeLockedTVar
       ) where

import           Universum

import           Pos.Util.Concurrent (withMVar)

-- | 'LockedTVar' is a mutable variable which can store a value
-- inside. It can be used to store a value from the outside world in
-- memory.
--
-- The problem is that we want to avoid situation when only one value
-- is updated and other code doesn't notice it. 'MVar' is taken when
-- we want to update this value. Before we update external value, we
-- put 'Nothing' into this variable, which means that in-memory value
-- is not available. Then we update external value, put it into this
-- variable and release 'MVar'.
--
-- So there are 3 possible states.
-- • Value is present, 'MVar' is not taken, we can just use the stored
-- value, it's synchronized with external value.
-- • 'MVar' is taken, it means that the value is being updated, we
-- should wait until update finishes.
-- • 'MVar' is not taken, but the value is absent. It means that the
-- value should be read from external source.
newtype LockedTVar a = LockedTVar (MVar (TVar (Maybe a)))

-- | Create a new 'LockedTVar' which is corresponds to the third state
-- from its description, i. e. external value is not known and should
-- be fetched directly from the outside.
newLockedTVar :: MonadIO m => m (LockedTVar a)
newLockedTVar = do
    tVar <- newTVarIO Nothing
    LockedTVar <$> newMVar tVar

-- | This function takes a 'LockedTVar' and an action. This action
-- writes a value to external storage and returns it. Then this value
-- is put into 'LockedTVar'.
writeLockedTVar :: (MonadIO m, MonadMask m) => LockedTVar a -> m a -> m a
writeLockedTVar (LockedTVar mvar) writeVal =
    withMVar mvar impl
  where
    impl tvar = do
      atomically $ writeTVar tvar Nothing
      newVal <- writeVal
      atomically $ writeTVar tvar (Just newVal)
      return newVal
