module Pos.Util.Future
       ( FutureError(..)
       , newInitFuture
       ) where

import           Universum

import           System.IO.Unsafe (unsafeInterleaveIO)

data FutureError = FutureAlreadyFilled Text
    deriving Show

instance Exception FutureError

-- | 'newInitFuture' creates a thunk and a procedure to fill it. This can be
-- used to create a data structure and initialize it gradually while doing some
-- IO (e.g. accessing the database).
-- There are two contracts the caller must obey:
-- * the thunk isn't forced until the procedure to fill it was called.
--   Violation of this contract will either block the thread forever or
--   trigger the error "thread blocked indefinitely in an MVar operation".
-- * the procedure to fill the thunk is called at most once.
--   Violation of this contract will throw `FutureAlreadyFilled`.
--
-- You can provide a name to 'newInitFuture' to make debugging easier when
-- something goes wrong and e.g. a future get filled twice.
newInitFuture
    :: forall m m' a.
       (MonadIO m, MonadIO m')
    => Text -> m (a, a -> m' ())
newInitFuture name = do
    mvar <- newEmptyMVar
    thunk <- liftIO $ unsafeInterleaveIO (readMVar mvar)
    let setter value = assertSingleAssignment =<< tryPutMVar mvar value
    pure (thunk, setter)
  where
    assertSingleAssignment :: Bool -> m' ()
    assertSingleAssignment = \case
        True -> pure ()
        False -> liftIO $ throwM (FutureAlreadyFilled name)
