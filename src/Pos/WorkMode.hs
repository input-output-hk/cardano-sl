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
       , ncPublicKey
       , ncVssPublicKey
       , RealMode
       , ServiceMode
       , StatsMode
       , ProductionMode
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except     (ExceptT)
import           Control.TimeWarp.Logging (WithNamedLogger (..))
import           Control.TimeWarp.Rpc     (BinaryDialog, MonadDialog, MonadResponse,
                                           MonadTransfer, Transfer)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId)
import           Universum                hiding (catch)

import           Pos.Crypto               (PublicKey, SecretKey, VssKeyPair, VssPublicKey,
                                           toPublic, toVssPublicKey)
import           Pos.DHT                  (DHTResponseT, MonadMessageDHT (..),
                                           WithDefaultMsgHeader)
import           Pos.DHT.Real             (KademliaDHT)
import           Pos.Slotting             (MonadSlots (..))
import           Pos.State                (MonadDB (..), NodeState)
import           Pos.Statistics           (MonadStats, NoStatsT, StatsT)
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
      , MonadStats m
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

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (StatsT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext m) =>
         WithNodeContext (NoStatsT m) where
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
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is a basis for `WorkMode`s used to really run system.
type RealMode = KademliaDHT (ContextHolder (DBHolder (BinaryDialog Transfer)))

-- | ServiceMode is the mode in which support nodes work
type ServiceMode = KademliaDHT (BinaryDialog Transfer)

-- | ProductionMode is an instance of WorkMode which is used (unsurprisingly) in production.
type ProductionMode = NoStatsT RealMode

-- | StatsMode is used for remote benchmarking
type StatsMode = StatsT RealMode
