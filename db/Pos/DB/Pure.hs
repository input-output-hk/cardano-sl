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
       ) where

import           Universum

import           Control.Lens                 (at, makeLenses)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString              as BS
import           Data.Conduit                 (Source)
import qualified Data.Conduit.List            as CL
import qualified Data.Map                     as M
import qualified Ether

import           Pos.Binary.Class             (Bi)
import           Pos.DB.Class                 (DBIteratorClass (..), DBTag (..), IterType,
                                               iterKeyPrefix)
import           Pos.DB.Functions             (processIterEntry)

type DBPureMap = Map ByteString ByteString

data DBPure = DBPure
    { _pureBlockIndexDB :: DBPureMap
    , _pureGStateDB     :: DBPureMap
    , _pureLrcDB        :: DBPureMap
    , _pureMiscDB       :: DBPureMap
    }

makeLenses ''DBPure

tagToLens :: DBTag -> Lens' DBPure DBPureMap
tagToLens BlockIndexDB = pureBlockIndexDB
tagToLens GStateDB     = pureGStateDB
tagToLens LrcDB        = pureLrcDB
tagToLens MiscDB       = pureMiscDB

type MonadPureDB m
     = (Ether.MonadReader' DBPure m, MonadBaseControl IO m, MonadThrow m)

dbGetPureDefault :: MonadPureDB m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetPureDefault (tagToLens -> l) key = Ether.asks' $ view $ l . at key

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
    (dbPure :: DBPure) <- lift Ether.ask'
    let filtered :: [(ByteString, ByteString)]
        filtered = M.toList . filterPrefix $ dbPure ^. l
    deserialized <- catMaybes <$> mapM (processIterEntry @i) filtered
    CL.sourceList deserialized
