{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Redirects enable instances of some type classes.
-- Here it is related to type classes from 'Pos.DB.Class'.

module Pos.DB.Rocks.Redirect
       ( dbGetDefault
       , dbIterSourceDefault
       , dbPutDefault
       , dbWriteBatchDefault
       , dbDeleteDefault
       ) where

import           Universum

import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit                 (Source)
import qualified Database.RocksDB             as Rocks

import           Pos.Binary.Class             (Bi)
import           Pos.DB.BatchOp               (rocksWriteBatch)
import           Pos.DB.Class                 (DBIteratorClass, DBTag, IterKey, IterType,
                                               IterValue)
import           Pos.DB.Rocks.Functions       (rocksDelete, rocksGetBytes,
                                               rocksIterSource, rocksPutBytes)
import           Pos.DB.Rocks.Types           (MonadRealDB, getDBByTag)


dbGetDefault :: MonadRealDB m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetDefault tag key = getDBByTag tag >>= rocksGetBytes key

dbPutDefault :: MonadRealDB m => DBTag -> ByteString -> ByteString -> m ()
dbPutDefault tag key val = getDBByTag tag >>= rocksPutBytes key val

dbWriteBatchDefault :: MonadRealDB m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchDefault tag batch = getDBByTag tag >>= rocksWriteBatch batch

dbDeleteDefault :: MonadRealDB m => DBTag -> ByteString -> m ()
dbDeleteDefault tag key = getDBByTag tag >>= rocksDelete key

dbIterSourceDefault ::
       forall m i.
       ( MonadRealDB m
       , MonadResource m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> Source m (IterType i)
dbIterSourceDefault = rocksIterSource
