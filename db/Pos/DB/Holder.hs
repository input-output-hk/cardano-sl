{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of MonadDB.

module Pos.DB.Holder
       ( DBHolder (..)
       , runDBHolder
       ) where

import           Control.Lens                 (iso)
import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.Fix            (MonadFix)
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Resource (MonadResource)
import           Mockable                     (ChannelT, Counter, Distribution, Gauge,
                                               MFunctor', Mockable (liftMockable),
                                               Promise, SharedAtomicT, SharedExclusiveT,
                                               ThreadId, liftMockableWrappedM)
import           Serokell.Util.Lens           (WrappedM (..))
import           System.Wlog                  (CanLog, HasLoggerName)
import           Universum

import           Pos.DB.Class                 (MonadDB (..))
import           Pos.DB.Types                 (DB (..), NodeDBs (..))

newtype DBHolder m a = DBHolder
    { getDBHolder :: ReaderT (NodeDBs) m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadIO
               , MonadFail
               , HasLoggerName
               , CanLog
               , MonadFix
               )

instance Monad m => WrappedM (DBHolder m) where
    type UnwrappedM (DBHolder m) = ReaderT NodeDBs m
    _WrappedM = iso getDBHolder DBHolder

instance MonadBase IO m => MonadBase IO (DBHolder m) where
    liftBase = lift . liftBase

deriving instance MonadResource m => MonadResource (DBHolder m)

instance (MonadIO m, MonadThrow m) =>
         MonadDB (DBHolder m) where
    getNodeDBs = DBHolder $ ask
    usingReadOptions opts l (DBHolder rdr)
        = DBHolder $ local (over l (\db -> db {rocksReadOpts = opts})) rdr
    usingWriteOptions opts l (DBHolder rdr)
        = DBHolder $ local (over l (\db -> db {rocksWriteOpts = opts})) rdr

type instance ThreadId (DBHolder m) = ThreadId m
type instance Promise (DBHolder m) = Promise m
type instance SharedAtomicT (DBHolder m) = SharedAtomicT m
type instance Counter (DBHolder m) = Counter m
type instance Distribution (DBHolder m) = Distribution m
type instance SharedExclusiveT (DBHolder m) = SharedExclusiveT m
type instance Gauge (DBHolder m) = Gauge m
type instance ChannelT (DBHolder m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (NodeDBs) m) m
         , MFunctor' d (DBHolder m) (ReaderT (NodeDBs) m)
         ) => Mockable d (DBHolder m) where
    liftMockable = liftMockableWrappedM

-- | Execute 'DBHolder' action with given 'NodeState'.
runDBHolder :: NodeDBs -> DBHolder m a -> m a
runDBHolder nState = flip runReaderT nState . getDBHolder
