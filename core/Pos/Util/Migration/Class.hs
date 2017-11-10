{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Util.Migration.Class
       ( MonadicMigration (..)
       , VersionActions (..)
       , Kind (..)
       , migrateDataTo

       -- * Aux class
       , HasVersionFileName (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting          (bprint)

import           System.IO.Error     (IOError, isDoesNotExistError)


class VersionActions (MigrateFrom new) => MonadicMigration m new where
    type family MigrateFrom new :: *
    migrateM :: MigrateFrom new -> m new

class VersionActions dtype where
    kind :: Kind m dtype
    getDataCurrentVersion    :: MonadIO m => m dtype
    putDataCurrentVersion    :: MonadIO m => dtype -> m ()
    deleteDataCurrentVersion :: MonadIO m => Proxy dtype -> m ()

data Kind (m :: * -> *) dtype where
    Base      :: VersionActions dtype => Kind m dtype
    Extends   :: (VersionActions dtype, MonadicMigration m dtype)
              => Proxy (MigrateFrom dtype)
              -> Kind m dtype

class HasVersionFileName dtype where
    fileName :: Proxy dtype -> FilePath

data MigrationException
    = NoOneVersionFound
    deriving (Show)

instance Exception MigrationException

instance Buildable MigrationException where
    build NoOneVersionFound = bprint "No one file corresponding some version found"

type MigrationMonads m = (MonadIO m, MonadCatch m)

migrateDataTo
    :: forall lastv m .
       (MigrationMonads m, VersionActions lastv, VersionActions (MigrateFrom lastv))
    => m lastv
migrateDataTo = do
    lastVersion <- migrateSequentialVersions @lastv kind
    putDataCurrentVersion lastVersion
    deleteAllPreviousVersions @(MigrateFrom lastv)
    pure lastVersion

deleteAllPreviousVersions :: forall dtype m . (MonadIO m, VersionActions dtype) => m ()
deleteAllPreviousVersions = case kind @dtype of
    Base -> deleteDataCurrentVersion @dtype Proxy
    Extends _ -> do
        deleteDataCurrentVersion @dtype Proxy
        deleteAllPreviousVersions @(MigrateFrom dtype)

migrateSequentialVersions :: forall exp m . MigrationMonads m => Kind m exp -> m exp
migrateSequentialVersions expKind = case expKind of
    Base ->
        getDataCurrentVersion `catch` handlerBase
    Extends _ ->
        getDataCurrentVersion `catch` handlerExtends (kind @(MigrateFrom exp))
  where
    handlerBase :: IOError -> m cur
    handlerBase e
        | isDoesNotExistError e = throwM NoOneVersionFound
        | otherwise = throwM e

    handlerExtends
        :: (MigrationMonads m, MonadicMigration m exp)
        => Kind m (MigrateFrom exp)
        -> IOError
        -> m exp
    handlerExtends prevKind e
        | isDoesNotExistError e = do
            prevVersion <- migrateSequentialVersions prevKind
            migrateM @_ @exp prevVersion
        | otherwise = throwM e
