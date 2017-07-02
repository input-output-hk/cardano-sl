{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Pure database implementation using 'Data.Map'. More efficient
-- option would be something akin to muteble prefix
-- tree. http://hackage.haskell.org/package/bytestring-trie might do
-- better though it's immutable.

module Pos.DB.Pure
       ( DBPureMap
       , DBPure
       , pureBlockIndexDB
       , pureGStateDB
       , pureLrcDB
       , pureMiscDB
       , pureBlocksStorage

       , MonadPureDB
       , DBPureVar

       , dbGetPureDefault
       , dbIterSourcePureDefault
       , dbPutPureDefault
       , dbDeletePureDefault
       , dbWriteBatchPureDefault
       ) where

import           Universum

import           Control.Lens                 (at, makeLenses)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString              as BS
import           Data.Conduit                 (Source)
import qualified Data.Conduit.List            as CL
import           Data.Default                 (Default (..))
import qualified Data.Map                     as M
import qualified Database.RocksDB             as Rocks
import           Ether.Internal               (HasLens (..))

import           Pos.Binary.Class             (Bi)
import           Pos.Core                     (HeaderHash)
import           Pos.DB.Class                 (DBIteratorClass (..), DBTag (..), IterType,
                                               iterKeyPrefix)
import           Pos.DB.Functions             (processIterEntry)
import           Pos.Util.Concurrent.RWVar    as RWV

-- | Bytestring to Bytestring mapping mimicking rocks kv storage.
type DBPureMap = Map ByteString ByteString

-- | Pure database datatype. Includes 4 subdatabases mocking rocks dbs
-- and a map for storing blocks data (we store them in files directly
-- in real implementation).
data DBPure = DBPure
    { _pureBlockIndexDB  :: DBPureMap
    , _pureGStateDB      :: DBPureMap
    , _pureLrcDB         :: DBPureMap
    , _pureMiscDB        :: DBPureMap
    , _pureBlocksStorage :: Map HeaderHash ByteString
    }

makeLenses ''DBPure

instance Default DBPure where
    def = DBPure mempty mempty mempty mempty mempty

type DBPureVar = RWV.RWVar DBPure

tagToLens :: DBTag -> Lens' DBPure DBPureMap
tagToLens BlockIndexDB = pureBlockIndexDB
tagToLens GStateDB     = pureGStateDB
tagToLens LrcDB        = pureLrcDB
tagToLens MiscDB       = pureMiscDB

-- | Monad having access to the pure database.
type MonadPureDB ctx m =
    ( MonadReader ctx m
    , HasLens DBPureVar ctx DBPureVar
    , MonadMask m
    , MonadBaseControl IO m
    , MonadIO m
    )

----------------------------------------------------------------------------
-- MonadDBRead / MonadDB
----------------------------------------------------------------------------

dbGetPureDefault :: MonadPureDB ctx m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetPureDefault (tagToLens -> l) key =
    view (l . at key) <$> (view (lensOf @DBPureVar) >>= RWV.read)

dbIterSourcePureDefault ::
       ( MonadPureDB ctx m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i))
    => DBTag
    -> Proxy i
    -> Source (ResourceT m) (IterType i)
dbIterSourcePureDefault (tagToLens -> l) (_ :: Proxy i) = do
    let filterPrefix = M.filterWithKey $ \k _ -> iterKeyPrefix @i `BS.isPrefixOf` k
    (dbPureVar :: DBPureVar) <- lift $ view (lensOf @DBPureVar)
    (filtered :: [(ByteString, ByteString)]) <-
        M.toList . filterPrefix . (view l) <$> lift (RWV.read dbPureVar)
    deserialized <- catMaybes <$> mapM (processIterEntry @i) filtered
    CL.sourceList deserialized

dbPutPureDefault :: MonadPureDB ctx m => DBTag -> ByteString -> ByteString -> m ()
dbPutPureDefault (tagToLens -> l) key val =
    view (lensOf @DBPureVar) >>= flip RWV.modifyPure (l . at key .~ Just val)

dbDeletePureDefault :: MonadPureDB ctx m => DBTag -> ByteString -> m ()
dbDeletePureDefault (tagToLens -> l) key =
    view (lensOf @DBPureVar) >>= flip RWV.modifyPure (l . at key .~ Nothing)

dbWriteBatchPureDefault :: MonadPureDB ctx m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchPureDefault tag = mapM_ processOp
  where
    processOp (Rocks.Put k v) = dbPutPureDefault tag k v
    processOp (Rocks.Del k)   = dbDeletePureDefault tag k
