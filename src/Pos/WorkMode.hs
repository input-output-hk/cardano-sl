{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | WorkMode constraint.

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       -- * Ssc local data
       , SscLDImpl
       , runSscLDImpl

       -- * DB
       , DBHolder (..)
       , runDBHolder

       -- * Context
       , ContextHolder (..)
       , NodeContext (..)
       , WithNodeContext (..)
       , ncPublicKey
       , ncVssPublicKey
       , runContextHolder

       -- * Actual modes
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
import           Control.Monad.State         (StateT (StateT), get, put)
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
import           Universum

import           Pos.Crypto                  (PublicKey, SecretKey, VssKeyPair,
                                              VssPublicKey, toPublic, toVssPublicKey)
import           Pos.DHT                     (DHTResponseT, MonadMessageDHT (..),
                                              WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.LocalData     (MonadSscLD (..),
                                              SscLocalDataClass (sscEmptyLocalData))
import           Pos.Ssc.Class.Storage       (SscStorageMode)
import           Pos.Ssc.Class.Types         (Ssc (SscLocalData))
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
      , MonadSscLD ssc m
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
-- MonadSscLD
----------------------------------------------------------------------------

instance (Monad m, MonadSscLD ssc m) =>
         MonadSscLD ssc (DHTResponseT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

newtype SscLDImpl ssc m a = SscLDImpl
    { getSscLDImpl :: StateT (SscLocalData ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, WithNamedLogger, MonadDialog p, WithNodeContext, MonadJL,
                MonadDB ssc)

-- TODO: refactor!
instance MonadMask m =>
         MonadMask (SscLDImpl ssc m) where
    mask a =
        SscLDImpl . StateT $
        \s -> mask $ \u -> runStateT (getSscLDImpl $ a $ q u) s
      where
        q
            :: (m (a, SscLocalData ssc) -> m (a, SscLocalData ssc))
            -> SscLDImpl ssc m a
            -> SscLDImpl ssc m a
        q u (SscLDImpl (StateT b)) = SscLDImpl (StateT (u . b))
    uninterruptibleMask a =
        SscLDImpl . StateT $
        \s -> uninterruptibleMask $ \u -> runStateT (getSscLDImpl $ a $ q u) s
      where
        q
            :: (m (a, SscLocalData ssc) -> m (a, SscLocalData ssc))
            -> SscLDImpl ssc m a
            -> SscLDImpl ssc m a
        q u (SscLDImpl (StateT b)) = SscLDImpl (StateT (u . b))

instance Monad m => MonadSscLD ssc (SscLDImpl ssc m) where
    getLocalData = SscLDImpl get
    setLocalData d = SscLDImpl (put d)

runSscLDImpl
    :: forall ssc m a.
       (Monad m, SscLocalDataClass ssc)
    => SscLDImpl ssc m a -> m a
runSscLDImpl = flip evalStateT (sscEmptyLocalData @ssc) . getSscLDImpl @ssc

instance MonadBase IO m => MonadBase IO (SscLDImpl ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (SscLDImpl ssc) where
    type StT (SscLDImpl ssc) a = StT (StateT (SscLocalData ssc)) a
    liftWith = defaultLiftWith SscLDImpl getSscLDImpl
    restoreT = defaultRestoreT SscLDImpl

instance MonadBaseControl IO m => MonadBaseControl IO (SscLDImpl ssc m) where
    type StM (SscLDImpl ssc m) a = ComposeSt (SscLDImpl ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (SscLDImpl ssc m) = ThreadId m

instance MonadTransfer m =>
         MonadTransfer (SscLDImpl ssc m) where
    sendRaw addr req =
        SscLDImpl get >>=
        \ctx ->
             lift $
             sendRaw addr (hoist (flip evalStateT ctx . getSscLDImpl) req)
    listenRaw binding sink =
        SscLDImpl $
        fmap SscLDImpl $ listenRaw binding $ hoistRespCond getSscLDImpl sink
    close = lift . close

instance (MonadSscLD ssc m, Monad m) => MonadSscLD ssc (KademliaDHT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

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
type RealMode ssc = KademliaDHT (SscLDImpl ssc (ContextHolder (DBHolder ssc (Dialog BinaryP Transfer))))

-- | ServiceMode is the mode in which support nodes work
type ServiceMode = KademliaDHT (Dialog BinaryP Transfer)

-- | ProductionMode is an instance of WorkMode which is used (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RealMode ssc)

-- | StatsMode is used for remote benchmarking
type StatsMode ssc = StatsT (RealMode ssc)
