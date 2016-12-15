{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Default implementation of MonadDB.

module Pos.Modern.DB.Holder
       ( DBHolder
       , runDBHolder
       ) where

import           Control.Lens                 (iso, over)
import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Control  (ComposeSt, MonadBaseControl (..),
                                               MonadTransControl (..), StM,
                                               defaultLiftBaseWith, defaultLiftWith,
                                               defaultRestoreM, defaultRestoreT)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.TimeWarp.Rpc         (MonadDialog, MonadTransfer)
import           Control.TimeWarp.Timed       (MonadTimed, ThreadId)
import           Serokell.Util.Lens           (WrappedM (..))
import           System.Wlog                  (CanLog, HasLoggerName)
import           Universum

import           Pos.Modern.DB.Class          (MonadDB (..))
import           Pos.Modern.DB.Types          (DB (..), NodeDBs (..))
import qualified Pos.State.State              as Legacy


newtype DBHolder ssc m a = DBHolder
    { getDBHolder :: ReaderT (NodeDBs ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
                MonadCatch, MonadMask, MonadIO, HasLoggerName, CanLog, MonadDialog s p,
                Legacy.MonadDB ssc)

instance Monad m => WrappedM (DBHolder ssc m) where
    type UnwrappedM (DBHolder ssc m) = ReaderT (NodeDBs ssc) m
    _WrappedM = iso getDBHolder DBHolder

instance MonadBase IO m => MonadBase IO (DBHolder ssc m) where
    liftBase = lift . liftBase

instance MonadTransfer s m => MonadTransfer s (DBHolder ssc m)

deriving instance MonadResource m => MonadResource (DBHolder ssc m)

type instance ThreadId (DBHolder ssc m) = ThreadId m

instance MonadIO m =>
         MonadDB ssc (DBHolder ssc m) where
    getNodeDBs = DBHolder $ ask
    usingReadOptions opts l (DBHolder rdr)
        = DBHolder $ local (over l (\db -> db {rocksReadOpts = opts})) rdr
    usingWriteOptions opts l (DBHolder rdr)
        = DBHolder $ local (over l (\db -> db {rocksWriteOpts = opts})) rdr

instance MonadTransControl (DBHolder ssc) where
    type StT (DBHolder ssc) a = StT (ReaderT (NodeDBs ssc)) a
    liftWith = defaultLiftWith DBHolder getDBHolder
    restoreT = defaultRestoreT DBHolder

instance MonadBaseControl IO m => MonadBaseControl IO (DBHolder ssc m) where
    type StM (DBHolder ssc m) a = ComposeSt (DBHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Execute 'DBHolder' action with given 'NodeState'.
runDBHolder :: NodeDBs ssc -> DBHolder ssc m a -> m a
runDBHolder nState = flip runReaderT nState . getDBHolder
