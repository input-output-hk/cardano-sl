{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-| 'WorkMode' constraint. It is widely used in almost every our code.
    Simple alias for bunch of useful constraints. This module also
    contains new monads to extend functional capabilities inside do-block.
-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       -- * Tx local data
       , TxLDImpl
       , runTxLDImpl

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
       , ncPubKeyAddress
       , runContextHolder

       -- * Actual modes
       , ProductionMode
       , RawRealMode
       , ServiceMode
       , StatsMode
       ) where

import           Control.Concurrent.MVar     (withMVar)
import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow, catchAll)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)
import           Control.Monad.State         (StateT)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (BinaryP, Dialog, MonadDialog,
                                              MonadResponse (..), MonadTransfer (..),
                                              Transfer)
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Data.Default                (def)
import           Formatting                  (sformat, shown, (%))
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName, WithLogger,
                                              logWarning)
import           Universum

import           Pos.Crypto                  (PublicKey, SecretKey, toPublic)
import           Pos.DHT                     (DHTResponseT, MonadMessageDHT (..),
                                              WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Ssc.Class.LocalData     (MonadSscLD (..),
                                              SscLocalDataClass (sscEmptyLocalData))
import           Pos.Ssc.Class.Storage       (SscStorageMode)
import           Pos.Ssc.Class.Types         (Ssc (SscLocalData, SscNodeContext))
import           Pos.State                   (MonadDB (..), NodeState)
import           Pos.Statistics.MonadStats   (MonadStats, NoStatsT, StatsT)
import           Pos.Txp.LocalData           (MonadTxLD (..), TxLocalData (..))
import           Pos.Types                   (Address, Timestamp (..), makePubKeyAddress)
import           Pos.Util.JsonLog            (MonadJL (..), appendJL)

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ssc m
    = ( WithLogger m
      , MonadIO m
      , MonadTimed m
      , MonadMask m
      , MonadSlots m
      , MonadDB ssc m
      , MonadTxLD m
      , SscStorageMode ssc
      , SscLocalDataClass ssc
      , SscHelpersClass ssc
      , MonadSscLD ssc m
      , WithNodeContext ssc m
      , MonadMessageDHT m
      , WithDefaultMsgHeader m
      , MonadStats m
      , MonadJL m
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode m
    = ( WithLogger m
      , MonadTimed m
      , MonadMask m
      , MonadIO m
      , MonadMessageDHT m
      , WithDefaultMsgHeader m
      )

----------------------------------------------------------------------------
-- MonadTxLD
----------------------------------------------------------------------------

instance MonadTxLD m => MonadTxLD (NoStatsT m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

instance MonadTxLD m => MonadTxLD (StatsT m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

instance MonadTxLD m => MonadTxLD (DHTResponseT m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

instance MonadTxLD m => MonadTxLD (KademliaDHT m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

newtype TxLDImpl m a = TxLDImpl
    { getTxLDImpl :: ReaderT (STM.TVar TxLocalData) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask)

type instance ThreadId (TxLDImpl m) = ThreadId m

instance Monad m => WrappedM (TxLDImpl m) where
    type UnwrappedM (TxLDImpl m) = ReaderT (STM.TVar TxLocalData) m
    _WrappedM = iso getTxLDImpl TxLDImpl

instance MonadTransfer m => MonadTransfer (TxLDImpl m)

instance MonadBase IO m => MonadBase IO (TxLDImpl m) where
    liftBase = lift . liftBase

instance MonadTransControl TxLDImpl where
    type StT TxLDImpl a = StT (ReaderT (STM.TVar TxLocalData)) a
    liftWith = defaultLiftWith TxLDImpl getTxLDImpl
    restoreT = defaultRestoreT TxLDImpl

instance MonadBaseControl IO m => MonadBaseControl IO (TxLDImpl m) where
    type StM (TxLDImpl m) a = ComposeSt TxLDImpl m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance MonadIO m =>
         MonadTxLD (TxLDImpl m) where
    getTxLocalData = atomically . STM.readTVar =<< TxLDImpl ask
    setTxLocalData d = atomically . flip STM.writeTVar d =<< TxLDImpl ask

runTxLDImpl :: MonadIO m => TxLDImpl m a -> m a
runTxLDImpl action = do
  ref <- liftIO $ STM.newTVarIO def
  flip runReaderT ref . getTxLDImpl $ action


----------------------------------------------------------------------------
-- MonadSscLD
----------------------------------------------------------------------------

instance (Monad m, MonadSscLD ssc m) =>
         MonadSscLD ssc (DHTResponseT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

instance MonadSscLD ssc m => MonadSscLD ssc (TxLDImpl m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

newtype SscLDImpl ssc m a = SscLDImpl
    { getSscLDImpl :: ReaderT (STM.TVar (SscLocalData ssc)) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog)

instance Monad m => WrappedM (SscLDImpl ssc m) where
    type UnwrappedM (SscLDImpl ssc m) = ReaderT (STM.TVar (SscLocalData ssc)) m
    _WrappedM = iso getSscLDImpl SscLDImpl

monadMaskHelper
    :: (ReaderT (STM.TVar (SscLocalData ssc)) m a -> ReaderT (STM.TVar (SscLocalData ssc)) m a)
    -> SscLDImpl ssc m a
    -> SscLDImpl ssc m a
monadMaskHelper u (SscLDImpl b) = SscLDImpl (u b)

instance MonadMask m =>
         MonadMask (SscLDImpl ssc m) where
    mask a = SscLDImpl $ mask $ \u -> getSscLDImpl $ a $ monadMaskHelper u
    uninterruptibleMask a =
        SscLDImpl $
        uninterruptibleMask $ \u -> getSscLDImpl $ a $ monadMaskHelper u

instance MonadIO m =>
         MonadSscLD ssc (SscLDImpl ssc m) where
    getLocalData = atomically . STM.readTVar =<< SscLDImpl ask
    setLocalData d = atomically . flip STM.writeTVar d =<< SscLDImpl ask

runSscLDImpl
    :: forall ssc m a.
       (MonadIO m, SscLocalDataClass ssc)
    => SscLDImpl ssc m a -> m a
runSscLDImpl action = do
  ref <- liftIO $ STM.newTVarIO (sscEmptyLocalData @ssc)
  flip runReaderT ref . getSscLDImpl @ssc $ action

instance MonadBase IO m => MonadBase IO (SscLDImpl ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (SscLDImpl ssc) where
    type StT (SscLDImpl ssc) a = StT (ReaderT (STM.TVar (SscLocalData ssc))) a
    liftWith = defaultLiftWith SscLDImpl getSscLDImpl
    restoreT = defaultRestoreT SscLDImpl

instance MonadBaseControl IO m => MonadBaseControl IO (SscLDImpl ssc m) where
    type StM (SscLDImpl ssc m) a = ComposeSt (SscLDImpl ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (SscLDImpl ssc m) = ThreadId m

instance MonadTransfer m => MonadTransfer (SscLDImpl ssc m)

instance (MonadSscLD ssc m, Monad m) => MonadSscLD ssc (KademliaDHT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

----------------------------------------------------------------------------
-- MonadDB
----------------------------------------------------------------------------

-- | Holder for database.
newtype DBHolder ssc m a = DBHolder
    { getDBHolder :: ReaderT (NodeState ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, HasLoggerName, CanLog, MonadDialog p)

-- | Execute 'DBHolder' action with given 'NodeState'.
runDBHolder :: NodeState ssc -> DBHolder ssc m a -> m a
runDBHolder db = flip runReaderT db . getDBHolder

instance Monad m => WrappedM (DBHolder ssc m) where
    type UnwrappedM (DBHolder ssc m) = ReaderT (NodeState ssc) m
    _WrappedM = iso getDBHolder DBHolder

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

instance MonadTransfer m => MonadTransfer (DBHolder ssc m)

instance Monad m => MonadDB ssc (DBHolder ssc m) where
    getNodeState = DBHolder ask

instance (MonadDB ssc m, Monad m) => MonadDB ssc (KademliaDHT m) where
    getNodeState = lift getNodeState

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

-- | NodeContext contains runtime context of node.
data NodeContext ssc = NodeContext
    { -- | Time when system started working.
      ncSystemStart :: !Timestamp
    , -- | Secret key used for blocks creation.
      ncSecretKey   :: !SecretKey
    , ncTimeLord    :: !Bool
    , ncJLFile      :: !(Maybe (MVar FilePath))
    , ncDbPath      :: !(Maybe FilePath)
    , ncSscContext  :: !(SscNodeContext ssc)
    }

-- | Generate 'PublicKey' from 'SecretKey' of 'NodeContext'.
ncPublicKey :: NodeContext ssc -> PublicKey
ncPublicKey = toPublic . ncSecretKey

-- | Generate 'Address' from 'SecretKey' of 'NodeContext'
ncPubKeyAddress :: NodeContext ssc -> Address
ncPubKeyAddress = makePubKeyAddress . ncPublicKey

-- | Class for something that has 'NodeContext' inside.
class WithNodeContext ssc m | m -> ssc where
    getNodeContext :: m (NodeContext ssc)

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (KademliaDHT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ExceptT e m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (DHTResponseT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (StatsT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (NoStatsT m) where
    getNodeContext = lift getNodeContext

-- | Wrapper for monadic action which brings 'NodeContext'.
newtype ContextHolder ssc m a = ContextHolder
    { getContextHolder :: ReaderT (NodeContext ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, HasLoggerName, CanLog, MonadDB ssc, MonadDialog p)

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder ctx = flip runReaderT ctx . getContextHolder

instance Monad m => WrappedM (ContextHolder ssc m) where
    type UnwrappedM (ContextHolder ssc m) = ReaderT (NodeContext ssc) m
    _WrappedM = iso getContextHolder ContextHolder

instance MonadBase IO m => MonadBase IO (ContextHolder ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (ContextHolder ssc) where
    type StT (ContextHolder ssc) a = StT (ReaderT (NodeContext ssc)) a
    liftWith = defaultLiftWith ContextHolder getContextHolder
    restoreT = defaultRestoreT ContextHolder

instance MonadBaseControl IO m => MonadBaseControl IO (ContextHolder ssc m) where
    type StM (ContextHolder ssc m) a = ComposeSt (ContextHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (ContextHolder ssc m) = ThreadId m

instance MonadTransfer m => MonadTransfer (ContextHolder ssc m)

instance MonadResponse m => MonadResponse (ContextHolder ssc m) where
    replyRaw dat = ContextHolder $ replyRaw (hoist getContextHolder dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

instance Monad m => WithNodeContext ssc (ContextHolder ssc m) where
    getNodeContext = ContextHolder ask

instance MonadJL m => MonadJL (KademliaDHT m) where
    jlLog = lift . jlLog

instance MonadSlots m => MonadSlots (KademliaDHT m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

instance (MonadTimed m, Monad m) =>
         MonadSlots (ContextHolder ssc m) where
    getSystemStartTime = ContextHolder $ asks ncSystemStart
    getCurrentTime = Timestamp <$> currentTime

instance (MonadIO m, MonadCatch m, WithLogger m) => MonadJL (ContextHolder ssc m) where
    jlLog ev = ContextHolder (asks ncJLFile) >>= maybe (pure ()) doLog
      where
        doLog logFileMV =
          (liftIO . withMVar logFileMV $ flip appendJL ev)
            `catchAll` \e -> logWarning $ sformat ("Can't write to json log: " % shown) e


----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc = KademliaDHT (TxLDImpl (
                                       SscLDImpl ssc (
                                           ContextHolder ssc (
                                               DBHolder ssc (Dialog BinaryP Transfer)))))

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RawRealMode ssc)

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT (RawRealMode ssc)

-- | ServiceMode is the mode in which support nodes work
type ServiceMode = KademliaDHT (Dialog BinaryP Transfer)
