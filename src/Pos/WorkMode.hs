{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode
       , DBHolder (..)
       , NodeContext (..)
       , WithNodeContext (..)
       , ContextHolder (..)
       , MonadBenchmark (..)
       , BenchmarkT (..)
       , NoBenchmarkT (..)
       , ncPublicKey
       , ncVssPublicKey
       , RealMode
       , SupportMode
       , BenchMode
       , ProductionMode
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except     (ExceptT)
import           Control.Monad.Trans      (MonadTrans)
import           Control.TimeWarp.Logging (WithNamedLogger (..))
import           Control.TimeWarp.Rpc     (BinaryP (..), Dialog, MonadDialog,
                                           MonadResponse, MonadTransfer (..), Transfer,
                                           hoistRespCond)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId)
import           Universum                hiding (catch)

import           Pos.Crypto               (PublicKey, SecretKey, VssKeyPair, VssPublicKey,
                                           toPublic, toVssPublicKey)
import           Pos.DHT                  (DHTResponseT, MonadDHT, MonadMessageDHT (..),
                                           WithDefaultMsgHeader)
import           Pos.DHT.Real             (KademliaDHT)
import           Pos.Slotting             (MonadSlots (..))
import           Pos.State                (MonadDB (..), NodeState, addStatRecord,
                                           getStatRecords)
import           Pos.Types                (Timestamp (..))

type WorkMode m
    = ( WithNamedLogger m
      , MonadIO m
      , MonadTimed m
      , MonadMask m
      , MonadSlots m
      , MonadDB m
      , WithNodeContext m
      , MonadMessageDHT m
      , WithDefaultMsgHeader m
      , MonadBenchmark m
      )

type MinWorkMode m
    = ( WithNamedLogger m
      , MonadTimed m
      , MonadMask m
      , MonadIO m
      , MonadMessageDHT m
      , WithDefaultMsgHeader m
      )

----------------------------------------------------------------------------
-- MonadDB
----------------------------------------------------------------------------

newtype DBHolder m a = DBHolder
    { getDBHolder :: ReaderT NodeState m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, WithNamedLogger, MonadDialog p,
               MonadResponse)

type instance ThreadId (DBHolder m) = ThreadId m

instance MonadTransfer m => MonadTransfer (DBHolder m) where
    sendRaw addr req = lift $ sendRaw addr req
    listenRaw binding sink =
        DBHolder $ listenRaw binding $ hoistRespCond getDBHolder sink
    close = lift . close

instance Monad m => MonadDB (DBHolder m) where
    getNodeState = DBHolder ask

instance (MonadDB m, Monad m) => MonadDB (KademliaDHT m) where
    getNodeState = lift getNodeState

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

-- | NodeContext contains runtime context of node.
data NodeContext = NodeContext
    { -- | Time when system started working.
      ncSystemStart :: !Timestamp
    , -- | Secret key used for blocks creation.
      ncSecretKey   :: !SecretKey
    , -- | Vss key pair used for MPC.
      ncVssKeyPair  :: !VssKeyPair
    } deriving (Show)

ncPublicKey :: NodeContext -> PublicKey
ncPublicKey = toPublic . ncSecretKey

ncVssPublicKey :: NodeContext -> VssPublicKey
ncVssPublicKey = toVssPublicKey . ncVssKeyPair

class WithNodeContext m where
    getNodeContext :: m NodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (KademliaDHT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (ExceptT e m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (DHTResponseT m) where
    getNodeContext = lift getNodeContext

newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT NodeContext m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, WithNamedLogger, MonadDB, MonadDialog p, MonadResponse)

type instance ThreadId (ContextHolder m) = ThreadId m

instance MonadTransfer m => MonadTransfer (ContextHolder m) where
    sendRaw addr req = lift $ sendRaw addr req
    listenRaw binding sink =
        ContextHolder $ listenRaw binding $ hoistRespCond getContextHolder sink
    close = lift . close

instance Monad m => WithNodeContext (ContextHolder m) where
    getNodeContext = ContextHolder ask

instance MonadSlots m => MonadSlots (KademliaDHT m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

instance (MonadTimed m, Monad m) =>
         MonadSlots (ContextHolder m) where
    getSystemStartTime = ContextHolder $ asks ncSystemStart
    getCurrentTime = Timestamp <$> currentTime

----------------------------------------------------------------------------
-- Benchmarking
----------------------------------------------------------------------------

type CounterLabel = Text

class Monad m => MonadBenchmark m where
    type Measure m :: *

    logMeasure :: CounterLabel -> Measure m -> m ()
    getMeasures :: CounterLabel -> m (Maybe [Measure m])

-- TODO: is there a way to avoid such boilerplate for transformers?
instance MonadBenchmark m => MonadBenchmark (KademliaDHT m) where
    type Measure (KademliaDHT m) = Measure m
    logMeasure label = lift . logMeasure label
    getMeasures = lift . getMeasures

instance MonadBenchmark m => MonadBenchmark (ReaderT a m) where
    type Measure (ReaderT a m) = Measure m
    logMeasure label = lift . logMeasure label
    getMeasures = lift . getMeasures

instance MonadBenchmark m => MonadBenchmark (StateT a m) where
    type Measure (StateT a m) = Measure m
    logMeasure label = lift . logMeasure label
    getMeasures = lift . getMeasures

instance MonadBenchmark m => MonadBenchmark (ExceptT e m) where
    type Measure (ExceptT e m) = Measure m
    logMeasure label = lift . logMeasure label
    getMeasures = lift . getMeasures

instance MonadBenchmark m => MonadBenchmark (DHTResponseT m) where
    type Measure (DHTResponseT m) = Measure m
    logMeasure label = lift . logMeasure label
    getMeasures = lift . getMeasures

type instance ThreadId (NoBenchmarkT m) = ThreadId m
type instance ThreadId (BenchmarkT m) = ThreadId m

newtype NoBenchmarkT m a = NoBenchmarkT
    { getNoBenchmarkT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger,
               MonadDialog p, MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots,
               WithDefaultMsgHeader, WithNodeContext)

instance MonadTrans NoBenchmarkT where
    lift = NoBenchmarkT

instance MonadTransfer m => MonadTransfer (NoBenchmarkT m) where
    sendRaw addr req = lift $ sendRaw addr req
    listenRaw binding sink =
        NoBenchmarkT $ listenRaw binding $ hoistRespCond runNoBenchmarksT sink
    close = lift . close

instance Monad m => MonadBenchmark (NoBenchmarkT m) where
    type Measure (NoBenchmarkT m) = ()
    logMeasure _ _ = pure ()
    getMeasures _ = pure $ pure []

newtype BenchmarkT m a = BenchmarkT
    { getBenchmarkT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger,
               MonadDialog p, MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots,
               WithDefaultMsgHeader, WithNodeContext)

instance MonadTrans BenchmarkT where
    lift = BenchmarkT

instance MonadTransfer m => MonadTransfer (BenchmarkT m) where
    sendRaw addr req = lift $ sendRaw addr req
    listenRaw binding sink =
        BenchmarkT $ listenRaw binding $ hoistRespCond runBenchmarkT sink
    close = lift . close

instance (MonadIO m, MonadDB m) => MonadBenchmark (BenchmarkT m) where
    type Measure (BenchmarkT m) = (ByteString, Timestamp)
    logMeasure label = lift . uncurry (addStatRecord label)
    getMeasures = lift . getStatRecords

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is a basis for `WorkMode`s used to really run system.
type RealMode = KademliaDHT (ContextHolder (DBHolder (Dialog BinaryP Transfer)))

-- | SupportMode is the mode in which support nodes work
type SupportMode = KademliaDHT (BinaryDialog Transfer)

-- | ProductionMode is an instance of WorkMode which is used (unsurprisingly) in production.
type ProductionMode = NoBenchmarkT RealMode

-- | BenchMode is used for remote benchmarking
type BenchMode = BenchmarkT RealMode
