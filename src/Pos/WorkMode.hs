{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
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
       , runTxLDImplRaw

       -- * Socket state
       , SocketState

       -- * Actual modes
       , ProductionMode
       , RawRealMode
       , ServiceMode
       , StatsMode
       ) where


import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (Dialog, MonadDialog, MonadTransfer (..),
                                              Transfer)
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Data.Default                (def)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName, WithLogger)
import           Universum

import           Pos.Context                 (ContextHolder, WithNodeContext)
import           Pos.DHT                     (DHTPacking, DHTResponseT (..),
                                              MonadMessageDHT (..), WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT (..))
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB.Class         as Modern
import qualified Pos.Modern.DB.Holder        as Modern
#endif
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Ssc.Class.LocalData     (MonadSscLD (..), SscLocalDataClass)
import           Pos.Ssc.Class.Storage       (SscStorageMode)
import           Pos.Ssc.LocalData           (SscLDImpl)
import           Pos.State                   (DBHolder, MonadDB (..))
import           Pos.Statistics.MonadStats   (MonadStats, NoStatsT, StatsT)
import           Pos.Txp.LocalData           (MonadTxLD (..), TxLocalData (..))
import           Pos.Util.JsonLog            (MonadJL (..))

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ssc m
    = ( WithLogger m
      , MonadIO m
      , MonadTimed m
      , MonadMask m
      , MonadSlots m
      , MonadDB ssc m
#ifdef WITH_ROCKS
      , Modern.MonadDB ssc m
#endif
      , MonadTxLD m
      , SscStorageMode ssc
      , SscLocalDataClass ssc
      , SscHelpersClass ssc
      , MonadSscLD ssc m
      , WithNodeContext ssc m
      , MonadMessageDHT SocketState m
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
      , MonadMessageDHT SocketState m
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

instance MonadTxLD m => MonadTxLD (DHTResponseT s m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

instance MonadTxLD m => MonadTxLD (KademliaDHT m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

instance MonadTxLD m => MonadTxLD (ReaderT r m) where
    getTxLocalData = lift getTxLocalData
    setTxLocalData = lift . setTxLocalData

newtype TxLDImpl m a = TxLDImpl
    { getTxLDImpl :: ReaderT (STM.TVar TxLocalData) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask)

type instance ThreadId (TxLDImpl m) = ThreadId m

#ifdef WITH_ROCKS
deriving instance Modern.MonadDB ssc m => Modern.MonadDB ssc (TxLDImpl m)
#endif

instance Monad m => WrappedM (TxLDImpl m) where
    type UnwrappedM (TxLDImpl m) = ReaderT (STM.TVar TxLocalData) m
    _WrappedM = iso getTxLDImpl TxLDImpl

instance MonadTransfer s m => MonadTransfer s (TxLDImpl m)

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
runTxLDImpl action = liftIO (STM.newTVarIO def) >>= runTxLDImplRaw action

runTxLDImplRaw :: TxLDImpl m a -> STM.TVar TxLocalData -> m a
runTxLDImplRaw = runReaderT . getTxLDImpl

----------------------------------------------------------------------------
-- MonadSscLD
----------------------------------------------------------------------------

instance MonadSscLD ssc m => MonadSscLD ssc (TxLDImpl m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

----------------------------------------------------------------------------
-- HZ
----------------------------------------------------------------------------

instance MonadJL m => MonadJL (KademliaDHT m) where
    jlLog = lift . jlLog

----------------------------------------------------------------------------
-- Socket state
----------------------------------------------------------------------------

type SocketState = ()

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc = KademliaDHT (TxLDImpl (
                                       SscLDImpl ssc (
                                           ContextHolder ssc (
#ifdef WITH_ROCKS
                                               Modern.DBHolder ssc (
#endif
                                                   DBHolder ssc (Dialog DHTPacking (Transfer SocketState))))))
#ifdef WITH_ROCKS
                                   )
#endif

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RawRealMode ssc)

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT (RawRealMode ssc)

-- | ServiceMode is the mode in which support nodes work
type ServiceMode = KademliaDHT (Dialog DHTPacking (Transfer SocketState))
