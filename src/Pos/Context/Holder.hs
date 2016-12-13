{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Default implementation of WithNodeContext.

module Pos.Context.Holder
       ( ContextHolder (..)
       , runContextHolder
       ) where

import           Control.Concurrent.MVar     (withMVar)
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow, catchAll)
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)

import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadResponse (..),
                                              MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)

import           Formatting                  (sformat, shown, (%))
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName, WithLogger,
                                              logWarning)
import           Universum

#ifdef WITH_ROCKS
import qualified Pos.Modern.DB               as Modern
#endif
import           Pos.Context.Class           (WithNodeContext (..))
import           Pos.Context.Context         (NodeContext (..))
import           Pos.Slotting                (MonadSlots (..))
import           Pos.State                   (MonadDB (..))
import           Pos.Types                   (Timestamp (..))
import           Pos.Util.JsonLog            (MonadJL (..), appendJL)

-- | Wrapper for monadic action which brings 'NodeContext'.
newtype ContextHolder ssc m a = ContextHolder
    { getContextHolder :: ReaderT (NodeContext ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, HasLoggerName, CanLog, MonadDB ssc, MonadDialog p)

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder ctx = flip runReaderT ctx . getContextHolder

#ifdef WITH_ROCKS
deriving instance Modern.MonadDB ssc m => Modern.MonadDB ssc (ContextHolder ssc m)
#endif

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
