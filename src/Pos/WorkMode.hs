{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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

import           Control.Concurrent.MVar     (withMVar)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow, catchAll)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Logging    (WithNamedLogger (..), logWarning)
import           Control.TimeWarp.Rpc        (BinaryP, Dialog, MonadDialog, MonadResponse,
                                              MonadTransfer (..), Transfer, hoistRespCond)
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Formatting                  (sformat, shown, (%))
import           Universum                   hiding (catch)

import           Pos.Crypto                  (PublicKey, SecretKey, VssKeyPair,
                                              VssPublicKey, toPublic, toVssPublicKey)
import           Pos.DHT                     (DHTResponseT, MonadMessageDHT (..),
                                              WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.State                   (MonadDB (..), NodeState)
import           Pos.Statistics.MonadStats   (MonadStats, NoStatsT, StatsT)
import           Pos.Types                   (Timestamp (..))
import           Pos.Util.JsonLog            (MonadJL (..), appendJL)

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
      , MonadJL m
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

instance MonadBase IO m => MonadBase IO (DBHolder m) where
    liftBase = lift . liftBase

instance MonadTransControl DBHolder where
    type StT DBHolder a = StT (ReaderT NodeState) a
    liftWith = defaultLiftWith DBHolder getDBHolder
    restoreT = defaultRestoreT DBHolder

instance MonadBaseControl IO m => MonadBaseControl IO (DBHolder m) where
    type StM (DBHolder m) a = ComposeSt DBHolder m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

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
    , ncTimeLord    :: !Bool
    , ncJLFile      :: !(Maybe (MVar FilePath))
    }

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
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, WithNamedLogger, MonadDB, MonadDialog p, MonadResponse)

instance MonadBase IO m => MonadBase IO (ContextHolder m) where
    liftBase = lift . liftBase

instance MonadTransControl ContextHolder where
    type StT ContextHolder a = StT (ReaderT NodeContext) a
    liftWith = defaultLiftWith ContextHolder getContextHolder
    restoreT = defaultRestoreT ContextHolder

instance MonadBaseControl IO m => MonadBaseControl IO (ContextHolder m) where
    type StM (ContextHolder m) a = ComposeSt ContextHolder m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (ContextHolder m) = ThreadId m

instance MonadTransfer m => MonadTransfer (ContextHolder m) where
    sendRaw addr req = lift $ sendRaw addr req
    listenRaw binding sink =
        ContextHolder $ listenRaw binding $ hoistRespCond getContextHolder sink
    close = lift . close

instance Monad m => WithNodeContext (ContextHolder m) where
    getNodeContext = ContextHolder ask

instance MonadJL m => MonadJL (KademliaDHT m) where
    jlLog = lift . jlLog

instance MonadSlots m => MonadSlots (KademliaDHT m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

instance (MonadTimed m, Monad m) =>
         MonadSlots (ContextHolder m) where
    getSystemStartTime = ContextHolder $ asks ncSystemStart
    getCurrentTime = Timestamp <$> currentTime

instance (MonadIO m, WithNamedLogger m, MonadCatch m) => MonadJL (ContextHolder m) where
    jlLog ev = ContextHolder (asks ncJLFile) >>= maybe (pure ()) doLog
      where
        doLog logFileMV =
          (liftIO . withMVar logFileMV $ flip appendJL ev)
            `catchAll` \e -> logWarning $ sformat ("Can't write to json log: " % shown) e


----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is a basis for `WorkMode`s used to really run system.
type RealMode = KademliaDHT (ContextHolder (DBHolder (Dialog BinaryP Transfer)))

-- | ServiceMode is the mode in which support nodes work
type ServiceMode = KademliaDHT (Dialog BinaryP Transfer)

-- | ProductionMode is an instance of WorkMode which is used (unsurprisingly) in production.
type ProductionMode = NoStatsT RealMode

-- | StatsMode is used for remote benchmarking
type StatsMode = StatsT RealMode
