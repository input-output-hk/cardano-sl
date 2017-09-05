module Pos.Util.Future
       ( FutureError(..)
       , newInitFuture
       ) where

import           Universum

import           Control.Exception (throwIO)
import           System.IO.Unsafe  (unsafeInterleaveIO)

data FutureError = FutureAlreadyFilled
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
newInitFuture :: (MonadIO m, MonadIO m') => m (a, a -> m' ())
newInitFuture = do
    mvar <- newEmptyMVar
    thunk <- liftIO $ unsafeInterleaveIO (readMVar mvar)
    let setter value = assertSingleAssignment =<< tryPutMVar mvar value
    pure (thunk, setter)
  where
    assertSingleAssignment :: MonadIO m => Bool -> m ()
    assertSingleAssignment = \case
        True -> pure ()
        False -> liftIO $ throwIO FutureAlreadyFilled
