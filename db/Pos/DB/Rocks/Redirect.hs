{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Redirects enable instances of some type classes.
-- Here it is related to type classes from 'Pos.DB.Class'.

module Pos.DB.Rocks.Redirect
       ( DBRealRedirect
       , runDBRealRedirect
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import           Pos.DB.BatchOp               (rocksWriteBatch)
import           Pos.DB.Class                 (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks.Functions       (rocksDelete, rocksGetBytes,
                                               rocksIterSource, rocksPutBytes)
import           Pos.DB.Rocks.Types           (MonadRealDB, getDBByTag)


data DBRealRedirectTag

type DBRealRedirect = Ether.TaggedTrans DBRealRedirectTag IdentityT

runDBRealRedirect :: DBRealRedirect m a -> m a
runDBRealRedirect = coerce

instance (MonadRealDB m, t ~ IdentityT) =>
         MonadDBRead (Ether.TaggedTrans DBRealRedirectTag t m) where
    dbGet tag key = getDBByTag tag >>= rocksGetBytes key
    dbIterSource tag p = rocksIterSource tag p

instance (MonadRealDB m, t ~ IdentityT) =>
         MonadDB (Ether.TaggedTrans DBRealRedirectTag t m) where
    dbPut tag key val = getDBByTag tag >>= rocksPutBytes key val
    dbWriteBatch tag batch = getDBByTag tag >>= rocksWriteBatch batch
    dbDelete tag key = getDBByTag tag >>= rocksDelete key
