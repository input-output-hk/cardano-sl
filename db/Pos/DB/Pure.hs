{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Pure database implementation using 'Data.Map'. More efficient
-- option would be something akin to muteble prefix
-- tree. http://hackage.haskell.org/package/bytestring-trie might do
-- better though it's immutable.

module Pos.DB.Pure
       ( DBPureT
       ) where

import           Universum

import           Control.Lens      (at, makeLenses, uses)
import qualified Data.ByteString   as BS
import qualified Data.Conduit.List as CL
import qualified Data.Map          as M
import qualified Ether

import           Pos.DB.Class      (DBTag (..), MonadDBRead (..), iterKeyPrefix)
import           Pos.DB.Functions  (processIterEntry)
import           Pos.Util.Util     (ether)

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


type DBPureT m = Ether.StateT' DBPure m

instance (MonadThrow m) => MonadDBRead (DBPureT m) where
    dbGet (tagToLens -> l) key = ether $ use $ l . at key
    dbIterSource (tagToLens -> l) (_ :: Proxy i) = do
        let filterPrefix = M.filterWithKey $ \k _ -> iterKeyPrefix @i `BS.isPrefixOf` k
        (filtered :: [(ByteString, ByteString)]) <-
            lift $ ether $ uses l $ M.toList . filterPrefix
        deserialized <- catMaybes <$> mapM (processIterEntry @i) filtered
        CL.sourceList deserialized
