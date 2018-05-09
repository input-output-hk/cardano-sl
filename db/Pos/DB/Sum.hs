-- | Default implementation for DBSum.
-- DBSum is dynamic context,
-- representing one of two databases: Rocks and Pure.

module Pos.DB.Sum
       ( DBSum (..)
       , MonadDBSum
       , eitherDB
       -- * Default methods
       , dbGetSumDefault
       , dbIterSourceSumDefault
       , dbPutSumDefault
       , dbWriteBatchSumDefault
       , dbDeleteSumDefault
       ) where

import           Universum

import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (ConduitT, transPipe)

import qualified Database.RocksDB as Rocks
import           Pos.Binary.Class (Bi)
import           Pos.Core.Configuration (HasCoreConfiguration)
import           Pos.DB.Class (DBIteratorClass (..), DBTag, IterType)
import           Pos.DB.Pure (DBPureVar)
import qualified Pos.DB.Pure as DB
import           Pos.DB.Rocks (NodeDBs)
import qualified Pos.DB.Rocks as DB
import           Pos.Util.Util (HasLens (..))

data DBSum = RealDB NodeDBs | PureDB DBPureVar

type MonadDBSum ctx m =
    ( MonadReader ctx m
    , HasLens DBSum ctx DBSum
    , MonadMask m
    , MonadIO m
    )

eitherDB
    :: (MonadReader ctx m, HasLens DBSum ctx DBSum)
    => ReaderT NodeDBs m a -> ReaderT DBPureVar m a -> m a
eitherDB ract pact = view (lensOf @DBSum) >>= \case
    RealDB dbs -> runReaderT ract dbs
    PureDB pdb -> runReaderT pact pdb

dbGetSumDefault
    :: MonadDBSum ctx m
    => DBTag -> ByteString -> m (Maybe ByteString)
dbGetSumDefault tag key =
    eitherDB (DB.dbGetDefault tag key) (DB.dbGetPureDefault tag key)

dbIterSourceSumDefault
    :: ( MonadDBSum ctx m
       , MonadResource m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       , HasCoreConfiguration
       )
    => DBTag -> Proxy i -> ConduitT () (IterType i) m ()
dbIterSourceSumDefault tag proxy = view (lensOf @DBSum) >>= \case
    RealDB dbs -> transPipe (flip runReaderT dbs) (DB.dbIterSourceDefault tag proxy)
    PureDB pdb -> transPipe (flip runReaderT pdb) (DB.dbIterSourcePureDefault tag proxy)

dbPutSumDefault
    :: (MonadDBSum ctx m, HasCoreConfiguration)
    => DBTag -> ByteString -> ByteString -> m ()
dbPutSumDefault tag k v = eitherDB (DB.dbPutDefault tag k v) (DB.dbPutPureDefault tag k v)

dbWriteBatchSumDefault
    :: MonadDBSum ctx m
    => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchSumDefault tag b =
    eitherDB (DB.dbWriteBatchDefault tag b) (DB.dbWriteBatchPureDefault tag b)

dbDeleteSumDefault
    :: MonadDBSum ctx m
    => DBTag -> ByteString -> m ()
dbDeleteSumDefault tag k =
    eitherDB (DB.dbDeleteDefault tag k) (DB.dbDeletePureDefault tag k)
