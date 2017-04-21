{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of MonadDB.

module Pos.DB.Holder
       ( DBHolder
       , runDBHolder
       ) where

import qualified Control.Monad.Ether.Implicit as Ether
import           Pos.DB.Class                 (MonadDB (..))
import           Pos.DB.Types                 (DB (..), NodeDBs (..))
import           Pos.Util.Util                ()
import           Universum

type DBHolder = Ether.ReaderT NodeDBs

instance (MonadIO m, MonadCatch m) =>
         MonadDB (DBHolder m) where
    getNodeDBs = Ether.ask
    usingReadOptions opts l rdr
        = Ether.local (over l (\db -> db {rocksReadOpts = opts})) rdr
    usingWriteOptions opts l rdr
        = Ether.local (over l (\db -> db {rocksWriteOpts = opts})) rdr

-- | Execute 'DBHolder' action with given 'NodeState'.
runDBHolder :: NodeDBs -> DBHolder m a -> m a
runDBHolder = flip Ether.runReaderT
