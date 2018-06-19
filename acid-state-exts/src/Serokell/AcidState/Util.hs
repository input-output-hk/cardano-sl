-- | Some useful functions to work with Data.Acid.

module Serokell.AcidState.Util
       (
         -- | Simple helpers
         exceptStateToUpdate
       , exceptStateToUpdateGeneric
       , readerToQuery
       , stateToUpdate

         -- | Utilities
       , createAndDiscardArchive
       , tidyLocalState
       ) where

import           Control.Exception    (Exception, throw)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Reader (Reader, asks, runReader)
import           Control.Monad.State  (State, runState, state)
import           Control.Monad.Trans  (MonadIO (liftIO))
import           Data.Acid            (AcidState, Query, Update, createArchive,
                                       createCheckpoint)
import           System.Directory     (removeDirectoryRecursive)
import           System.FilePath      ((</>))

readerToQuery :: Reader s a -> Query s a
readerToQuery = asks . runReader

stateToUpdate :: State s a -> Update s a
stateToUpdate = state . runState

exceptStateToUpdate
    :: (Exception e)
    => ExceptT e (State s) a -> Update s a
exceptStateToUpdate = exceptStateToUpdateGeneric id

exceptStateToUpdateGeneric
  :: (Exception exc)
  => (e -> exc) -> ExceptT e (State s) a -> Update s a
exceptStateToUpdateGeneric toException u =
    state $
    runState $
    do res <- runExceptT u
       either (throw . toException) return res

-- | Archive unnecessary data (see createArchive docs for details) and
-- discard it. Works for local state.
createAndDiscardArchive :: MonadIO m => AcidState st -> FilePath -> m ()
createAndDiscardArchive st path =
    liftIO $ createArchive st >> removeDirectoryRecursive (path </> "Archive")

-- | Apply all updates and remove all data from local state which is
-- unnecessary for state restoration.
tidyLocalState :: MonadIO m => AcidState st -> FilePath -> m ()
tidyLocalState st path =
    liftIO (createCheckpoint st) >> createAndDiscardArchive st path
