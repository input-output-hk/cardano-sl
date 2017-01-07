{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of MonadDB.

module Pos.DB.Holder
       ( DBHolder (..)
       , runDBHolder
       ) where

import           Control.Lens                 (iso, over)
import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Resource (MonadResource)
import           Mockable                     (ChannelT, MFunctor' (hoist'),
                                               Mockable (liftMockable), Promise,
                                               SharedAtomicT, ThreadId, Throw)
import           Serokell.Util.Lens           (WrappedM (..))
import           System.Wlog                  (CanLog, HasLoggerName)
import           Universum

import           Pos.DB.Class                 (MonadDB (..))
import           Pos.DB.Types                 (DB (..), NodeDBs (..))

newtype DBHolder ssc m a = DBHolder
    { getDBHolder :: ReaderT (NodeDBs ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog)

instance Monad m => WrappedM (DBHolder ssc m) where
    type UnwrappedM (DBHolder ssc m) = ReaderT (NodeDBs ssc) m
    _WrappedM = iso getDBHolder DBHolder

instance MonadBase IO m => MonadBase IO (DBHolder ssc m) where
    liftBase = lift . liftBase

deriving instance MonadResource m => MonadResource (DBHolder ssc m)

instance (MonadIO m, Mockable Throw m) =>
         MonadDB ssc (DBHolder ssc m) where
    getNodeDBs = DBHolder $ ask
    usingReadOptions opts l (DBHolder rdr)
        = DBHolder $ local (over l (\db -> db {rocksReadOpts = opts})) rdr
    usingWriteOptions opts l (DBHolder rdr)
        = DBHolder $ local (over l (\db -> db {rocksWriteOpts = opts})) rdr

type instance ThreadId (DBHolder ssc m) = ThreadId m
type instance Promise (DBHolder ssc m) = Promise m
type instance SharedAtomicT (DBHolder ssc m) = SharedAtomicT m
type instance ChannelT (DBHolder ssc m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (NodeDBs ssc) m) m
         , MFunctor' d (DBHolder ssc m) (ReaderT (NodeDBs ssc) m)
         ) => Mockable d (DBHolder ssc m) where
    liftMockable dmt = DBHolder $ liftMockable $ hoist' getDBHolder dmt

-- | Execute 'DBHolder' action with given 'NodeState'.
runDBHolder :: NodeDBs ssc -> DBHolder ssc m a -> m a
runDBHolder nState = flip runReaderT nState . getDBHolder
