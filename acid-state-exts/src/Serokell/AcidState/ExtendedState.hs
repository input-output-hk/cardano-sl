{-# LANGUAGE TypeFamilies #-}

-- | The idea of ExtendedState is to store information about location
-- of the state (either FilePath or memory).

module Serokell.AcidState.ExtendedState
       ( ExtendedState (..)
       , closeExtendedState
       , extendedStateToAcid
       , openLocalExtendedState
       , openMemoryExtendedState
       , queryExtended
       , tidyExtendedState
       , updateExtended
       ) where

import           Control.Monad.Extra     (whenM)
import           Control.Monad.Trans     (MonadIO (liftIO))
import           Data.Acid               (AcidState, EventResult, EventState, IsAcidic,
                                          QueryEvent, UpdateEvent, closeAcidState,
                                          openLocalStateFrom)
import           Data.Acid.Advanced      (query', update')
import           Data.Acid.Memory        (openMemoryState)
import           Data.Typeable           (Typeable)

import           System.Directory        (doesDirectoryExist, removeDirectoryRecursive)

import           Serokell.AcidState.Util (tidyLocalState)

-- | ExtendedState is like usual AcidState, but also stores
-- information about FilePath (unless it's in memory).
data ExtendedState st
    = ESLocal (AcidState st)
              FilePath
    | ESMemory (AcidState st)

-- | Convert ExtendedState to AcidState.
extendedStateToAcid :: ExtendedState st -> AcidState st
extendedStateToAcid (ESLocal s _) = s
extendedStateToAcid (ESMemory s)  = s

-- | Like query', but works on ExtendedState.
queryExtended
    :: (EventState event ~ st, QueryEvent event, MonadIO m)
    => ExtendedState st -> event -> m (EventResult event)
queryExtended st = query' (extendedStateToAcid st)

-- | Like update', but works on ExtendedState.
updateExtended
    :: (EventState event ~ st, UpdateEvent event, MonadIO m)
    => ExtendedState st -> event -> m (EventResult event)
updateExtended st = update' (extendedStateToAcid st)

-- | Like openLocalStateFrom, but returns ExtendedState and operates
-- in MonadIO.
openLocalExtendedState
    :: (IsAcidic st, Typeable st, MonadIO m)
    => Bool -> FilePath -> st -> m (ExtendedState st)
openLocalExtendedState deleteIfExists fp st = do
    whenM ((deleteIfExists &&) <$> liftIO (doesDirectoryExist fp)) $
        liftIO $ removeDirectoryRecursive fp
    liftIO $ flip ESLocal fp <$> openLocalStateFrom fp st

-- | Like openMemoryState, but returns ExtendedState and operates in
-- MonadIO.
openMemoryExtendedState
    :: (IsAcidic st, Typeable st, MonadIO m)
    => st -> m (ExtendedState st)
openMemoryExtendedState st = liftIO $ ESMemory <$> openMemoryState st

-- | Like closeAcidState, but operates on ExtendedState and in
-- MonadIO.
closeExtendedState :: MonadIO m => ExtendedState st -> m ()
closeExtendedState = liftIO . closeAcidState . extendedStateToAcid

-- | Like tidyLocalState, but operates on ExtendedState.
tidyExtendedState :: MonadIO m => ExtendedState st -> m ()
tidyExtendedState (ESLocal st fp) = tidyLocalState st fp
tidyExtendedState (ESMemory _)    = return ()
