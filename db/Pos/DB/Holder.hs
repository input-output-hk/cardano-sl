{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of MonadDB.

module Pos.DB.Holder
       ( DBHolder
       , runDBHolder
       ) where

import qualified Ether
import           Pos.DB.Types  (NodeDBs)
import           Pos.Util.Util ()
import           Universum

type DBHolder = Ether.ReaderT' NodeDBs

-- | Execute 'DBHolder' action with given 'NodeState'.
runDBHolder :: NodeDBs -> DBHolder m a -> m a
runDBHolder = flip Ether.runReaderT'
