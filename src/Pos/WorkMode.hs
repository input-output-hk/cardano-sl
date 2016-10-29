{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , WorkIOMode
       , DBHolder (..)
       , NodeContext (..)
       , WithNodeContext (..)
       , ContextHolder (..)
       , MonadBenchmark (..)
       , BenchmarkT (..)
       , NoBenchmarkT (..)
       , ncPublicKey
       , ncVssPublicKey
       , SemiRealMode
       , RealMode
       , BenchMode
       , RealWithoutNetwork
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except     (ExceptT)
import           Control.Monad.Trans      (MonadTrans)
import           Control.TimeWarp.Logging (WithNamedLogger (..))
import           Control.TimeWarp.Rpc     (BinaryDialog, MonadDialog, MonadResponse,
                                           MonadTransfer, Transfer)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId)
import           Universum                hiding (catch)

import           Pos.Crypto               (PublicKey, SecretKey, VssKeyPair, VssPublicKey,
                                           toPublic, toVssPublicKey)
import           Pos.DHT                  (DHTResponseT, MonadDHT, MonadMessageDHT (..),
                                           WithDefaultMsgHeader)
import           Pos.DHT.Real             (KademliaDHT)
import           Pos.Slotting             (MonadSlots (..), Timestamp (..))
import           Pos.State                (MonadDB (..), NodeState, addStatRecord,
                                           getStatRecords)

type WorkIOMode m
    = ( WithNamedLogger m
      , MonadIO m
      , MonadTimed m
      , MonadMask m
      )

type WorkMode m
    = ( WorkIOMode m
      , MonadSlots m
      , MonadDB m
      , WithNodeContext m
      , MonadMessageDHT m
      , WithDefaultMsgHeader m
      , MonadBenchmark m
      )

----------------------------------------------------------------------------
-- MonadDB
----------------------------------------------------------------------------

newtype DBHolder m a = DBHolder
    { getDBHolder :: ReaderT NodeState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, WithNamedLogger, MonadTransfer, MonadDialog, MonadResponse)

type instance ThreadId (DBHolder m) = ThreadId m

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
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, WithNamedLogger, MonadDB, MonadTransfer, MonadDialog, MonadResponse)

type instance ThreadId (ContextHolder m) = ThreadId m

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
    { runNoBenchmarksT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger, MonadTransfer, MonadDialog,
               MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots, WithDefaultMsgHeader,
               WithNodeContext)

instance MonadTrans NoBenchmarkT where
    lift = NoBenchmarkT

instance Monad m => MonadBenchmark (NoBenchmarkT m) where
    type Measure (NoBenchmarkT m) = ()
    logMeasure _ _ = pure ()
    getMeasures _ = pure $ pure []

newtype BenchmarkT m a = BenchmarkT
    { runBenchmarkT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger, MonadTransfer, MonadDialog,
               MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots, WithDefaultMsgHeader,
               WithNodeContext)

instance MonadTrans BenchmarkT where
    lift = BenchmarkT

instance (MonadIO m, MonadDB m) => MonadBenchmark (BenchmarkT m) where
    type Measure (BenchmarkT m) = (ByteString, Timestamp)
    logMeasure label = lift . uncurry (addStatRecord label)
    getMeasures = lift . getStatRecords

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | Type alias to for part of RealMode without network
type RealWithoutNetwork = ContextHolder (DBHolder (BinaryDialog Transfer))

-- | SemiRealMode is a WorkMode which allows us to choose benchmarking mode.
-- TODO: Such disposition of transformers is required by code in `Pos.DHT.Real`.
-- This leads to uglyness, thus it should be refactored later
type SemiRealMode m = KademliaDHT (m RealWithoutNetwork)

-- | RealMode is an instance of WorkMode which can be used to really run system.
type RealMode = SemiRealMode NoBenchmarkT

-- | BenchMode is used for remote benchmarking
type BenchMode = SemiRealMode BenchmarkT
