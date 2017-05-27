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

import           Pos.DB.Class                 (MonadDB, MonadDBPure (..), dbTagToLens,
                                               getNodeDBs)
import           Pos.DB.Functions             (rocksGetBytes)

data DBPureRedirectTag

type DBPureRedirect =
    Ether.TaggedTrans DBPureRedirectTag IdentityT

runDBPureRedirect :: DBPureRedirect m a -> m a
runDBPureRedirect = coerce

instance
    (MonadDB m, t ~ IdentityT) =>
        MonadDBPure (Ether.TaggedTrans DBPureRedirectTag t m)
  where
    dbGet tag key = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksGetBytes key db
