{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Modern implementation of node's persistent state.

module Pos.Modern.WorkMode
       ( runDBHolder
       ) where

import           Control.Lens                 (over)
import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Control  (ComposeSt, MonadBaseControl (..),
                                               MonadTransControl (..), StM,
                                               defaultLiftBaseWith, defaultLiftWith,
                                               defaultRestoreM, defaultRestoreT)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.TimeWarp.Timed       (MonadTimed, ThreadId)
import           System.Wlog                  (CanLog, HasLoggerName)
import           Universum

import           Pos.Modern.State.Storage     (DB (..), MonadDB (..), NodeState (..),
                                               blockDb, utxoDb)


newtype DBHolder ssc m a = DBHolder
    { getDBHolder :: ReaderT (NodeState ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, HasLoggerName, CanLog)

instance MonadBase IO m => MonadBase IO (DBHolder ssc m) where
    liftBase = lift . liftBase

deriving instance MonadResource m => MonadResource (DBHolder ssc m)

type instance ThreadId (DBHolder ssc m) = ThreadId m

instance MonadIO m =>
         MonadDB ssc (DBHolder ssc m) where
    getBlockDB = DBHolder $ asks _blockDb
    getUtxoDB = DBHolder $ asks _utxoDb
    getUndoDB = DBHolder $ asks _blockDb
    usingReadOptionsUtxo  opts (DBHolder rdr)
        = DBHolder $ local (over utxoDb (\db -> db {rocksReadOpts = opts})) rdr
    usingWriteOptionsUtxo  opts (DBHolder rdr)
        = DBHolder $ local (over utxoDb (\db -> db {rocksWriteOpts = opts})) rdr
    usingReadOptionsBlock  opts (DBHolder rdr)
        = DBHolder $ local (over blockDb (\db -> db {rocksReadOpts = opts})) rdr
    usingWriteOptionsBlock  opts (DBHolder rdr)
        = DBHolder $ local (over blockDb (\db -> db {rocksWriteOpts = opts})) rdr

instance MonadTransControl (DBHolder ssc) where
    type StT (DBHolder ssc) a = StT (ReaderT (NodeState ssc)) a
    liftWith = defaultLiftWith DBHolder getDBHolder
    restoreT = defaultRestoreT DBHolder

instance MonadBaseControl IO m => MonadBaseControl IO (DBHolder ssc m) where
    type StM (DBHolder ssc m) a = ComposeSt (DBHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Execute 'DBHolder' action with given 'NodeState'.
runDBHolder :: NodeState ssc -> DBHolder ssc m a -> m a
runDBHolder nState = flip runReaderT nState . getDBHolder

-- Garbage

-- type TmpWorkMode ssc m = (MonadResourceBase m, MonadDB ssc m)

-- type TmpRealMode ssc = DBHolder ssc (ResourceT IO)

-- runTmpRealMode :: DB ssc -> TmpRealMode ssc a -> IO a
-- runTmpRealMode db = runResourceT . runDBHolder db
