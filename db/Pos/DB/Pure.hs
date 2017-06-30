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
       , MonadPureDB

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
import qualified Data.Map                     as M
import qualified Database.RocksDB             as Rocks
import qualified Ether

import           Pos.Binary.Class             (Bi)
import           Pos.DB.Class                 (DBIteratorClass (..), DBTag (..), IterType,
                                               iterKeyPrefix)
import           Pos.DB.Functions             (processIterEntry)
import           Pos.Util.Concurrent.RWVar    as RWV

type DBPureMap = Map ByteString ByteString

data DBPure = DBPure
    { _pureBlockIndexDB :: DBPureMap
    , _pureGStateDB     :: DBPureMap
    , _pureLrcDB        :: DBPureMap
    , _pureMiscDB       :: DBPureMap
    }

type DBPureVar = RWV.RWVar DBPure

makeLenses ''DBPure

tagToLens :: DBTag -> Lens' DBPure DBPureMap
tagToLens BlockIndexDB = pureBlockIndexDB
tagToLens GStateDB     = pureGStateDB
tagToLens LrcDB        = pureLrcDB
tagToLens MiscDB       = pureMiscDB

type MonadPureDB m =
    ( Ether.MonadReader' DBPureVar m
    , MonadMask m
    , MonadBaseControl IO m
    , MonadIO m
    )

dbGetPureDefault :: MonadPureDB m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetPureDefault (tagToLens -> l) key =
    view (l . at key) <$> (Ether.ask' >>= RWV.read)

dbIterSourcePureDefault ::
       forall m i.
       ( MonadPureDB m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> Source (ResourceT m) (IterType i)
dbIterSourcePureDefault (tagToLens -> l) (_ :: Proxy i) = do
    let filterPrefix = M.filterWithKey $ \k _ -> iterKeyPrefix @i `BS.isPrefixOf` k
    (dbPureVar :: DBPureVar) <- lift Ether.ask'
    (filtered :: [(ByteString, ByteString)]) <-
        M.toList . filterPrefix . (view l) <$> lift (RWV.read dbPureVar)
    deserialized <- catMaybes <$> mapM (processIterEntry @i) filtered
    CL.sourceList deserialized

dbPutPureDefault :: MonadPureDB m => DBTag -> ByteString -> ByteString -> m ()
dbPutPureDefault (tagToLens -> l) key val =
    Ether.ask' >>= flip RWV.modifyPure (l . at key .~ Just val)

dbDeletePureDefault :: MonadPureDB m => DBTag -> ByteString -> m ()
dbDeletePureDefault (tagToLens -> l) key =
    Ether.ask' >>= flip RWV.modifyPure (l . at key .~ Nothing)

dbWriteBatchPureDefault :: MonadPureDB m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchPureDefault tag = mapM_ processOp
  where
    processOp (Rocks.Put k v) = dbPutPureDefault tag k v
    processOp (Rocks.Del k)   = dbDeletePureDefault tag k
