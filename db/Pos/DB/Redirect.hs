{-# LANGUAGE TypeFamilies #-}

-- | Redirects enable instances of some type classes.
-- Here it is related to type classes from 'Pos.DB.Class'.

module Pos.DB.Redirect
       ( DBPureRedirect
       , runDBPureRedirect
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import           Pos.DB.BatchOp               (rocksWriteBatch)
import           Pos.DB.Class                 (MonadDB (..), MonadDBRead (..),
                                               MonadRealDB, dbTagToLens, getNodeDBs)
import           Pos.DB.Functions             (rocksDelete, rocksGetBytes, rocksPutBytes)

data DBPureRedirectTag

type DBPureRedirect =
    Ether.TaggedTrans DBPureRedirectTag IdentityT

runDBPureRedirect :: DBPureRedirect m a -> m a
runDBPureRedirect = coerce

instance
    (MonadRealDB m, t ~ IdentityT) =>
        MonadDBRead (Ether.TaggedTrans DBPureRedirectTag t m)
  where
    dbGet tag key = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksGetBytes key db

instance
    (MonadRealDB m, t ~ IdentityT) =>
        MonadDB (Ether.TaggedTrans DBPureRedirectTag t m)
  where
    dbPut tag key val = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksPutBytes key val db
    dbWriteBatch tag batch = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksWriteBatch batch db
    dbDelete tag key = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksDelete key db
