{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-| 'WorkMode' constraint. It is widely used in almost every our code.
    Simple alias for bunch of useful constraints. This module also
    contains new monads to extend functional capabilities inside do-block.
-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode
       , DBHolder (..)
       , NodeContext (..)
       , WithNodeContext (..)
       , ContextHolder (..)
       , runDBHolder
       , runContextHolder
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
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (BinaryP, Dialog, MonadDialog,
                                              MonadResponse (..), MonadTransfer (..),
                                              Transfer, hoistRespCond)
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Formatting                  (sformat, shown, (%))
import           System.Wlog                 (WithNamedLogger (..), logWarning)
import           Universum                   hiding (catch)

import           Pos.Crypto                  (PublicKey, SecretKey, VssKeyPair,
                                              VssPublicKey, toPublic, toVssPublicKey)
import           Pos.DHT                     (DHTResponseT, MonadMessageDHT (..),
                                              WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.Storage       (SscStorageMode)
import           Pos.State                   (MonadDB (..), NodeState)
import           Pos.Statistics.MonadStats   (MonadStats, NoStatsT, StatsT)
import           Pos.Types                   (Timestamp (..))
import           Pos.Util.JsonLog            (MonadJL (..), appendJL)

type WorkMode ssc m
    = ( WithNamedLogger m
      , MonadIO m
      , MonadTimed m
      , MonadMask m
      , MonadSlots m
      , MonadDB ssc m
      , SscStorageMode ssc
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

newtype DBHolder ssc m a = DBHolder
    { getDBHolder :: ReaderT (NodeState ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, WithNamedLogger, MonadDialog p)

runDBHolder :: NodeState ssc -> DBHolder ssc m a -> m a
runDBHolder db = flip runReaderT db . getDBHolder

instance MonadBase IO m => MonadBase IO (DBHolder ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (DBHolder ssc) where
    type StT (DBHolder ssc) a = StT (ReaderT (NodeState ssc)) a
    liftWith = defaultLiftWith DBHolder getDBHolder
    restoreT = defaultRestoreT DBHolder

instance MonadBaseControl IO m => MonadBaseControl IO (DBHolder ssc m) where
    type StM (DBHolder ssc m) a = ComposeSt (DBHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (DBHolder ssc m) = ThreadId m

instance MonadTransfer m => MonadTransfer (DBHolder ssc m) where
    sendRaw addr req = DBHolder ask >>= \ctx -> lift $ sendRaw addr (hoist (runDBHolder ctx) req)
    listenRaw binding sink =
        DBHolder $ fmap DBHolder $ listenRaw binding $ hoistRespCond getDBHolder sink
    close = lift . close

instance Monad m => MonadDB ssc (DBHolder ssc m) where
    getNodeState = DBHolder ask

instance (MonadDB ssc m, Monad m) => MonadDB ssc (KademliaDHT m) where
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
    , ncDbPath      :: !(Maybe FilePath)
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
               MonadCatch, MonadMask, MonadIO, WithNamedLogger, MonadDB ssc, MonadDialog p)

runContextHolder :: NodeContext -> ContextHolder m a -> m a
runContextHolder ctx = flip runReaderT ctx . getContextHolder

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
    sendRaw addr req = ContextHolder ask >>= \ctx -> lift $ sendRaw addr (hoist (runContextHolder ctx) req)
    listenRaw binding sink =
        ContextHolder $ fmap ContextHolder $ listenRaw binding $ hoistRespCond getContextHolder sink
    close = lift . close

instance MonadResponse m => MonadResponse (ContextHolder m) where
    replyRaw dat = ContextHolder $ replyRaw (hoist getContextHolder dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

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
type RealMode ssc = KademliaDHT (ContextHolder (DBHolder ssc (Dialog BinaryP Transfer)))

-- | ServiceMode is the mode in which support nodes work
type ServiceMode = KademliaDHT (Dialog BinaryP Transfer)

-- | ProductionMode is an instance of WorkMode which is used (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RealMode ssc)

-- | StatsMode is used for remote benchmarking
type StatsMode ssc = StatsT (RealMode ssc)
