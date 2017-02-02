{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of WithNodeContext.

module Pos.Context.Holder
       ( ContextHolder (..)
       , runContextHolder
       ) where

import           Control.Concurrent.MVar   (withMVar)
import           Control.Lens              (iso)
import           Control.Monad.Base        (MonadBase (..))
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Reader      (ReaderT (ReaderT), ask)
import           Control.Monad.Trans.Class (MonadTrans)
import           Formatting                (sformat, shown, (%))
import           Mockable                  (Catch, ChannelT, Counter, CurrentTime,
                                            Distribution, Gauge, MFunctor',
                                            Mockable (liftMockable), Promise,
                                            SharedAtomicT, SharedExclusiveT, ThreadId,
                                            catchAll, currentTime, liftMockableWrappedM)
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName, WithLogger, logWarning)
import           Universum                 hiding (catchAll)

import           Pos.Constants             (genesisSlotDuration)
import           Pos.Context.Class         (WithNodeContext (..))
import           Pos.Context.Context       (NodeContext (..))
import           Pos.Context.Functions     (readNtpData, readNtpLastSlot, readNtpMargin)
import           Pos.DB.Class              (MonadDB)
import           Pos.Slotting              (MonadSlots (..), getCurrentSlotUsingNtp)
import           Pos.Txp.Class             (MonadTxpLD)
import           Pos.Types                 (Timestamp (..))
import           Pos.Util.JsonLog          (MonadJL (..), appendJL)

-- | Wrapper for monadic action which brings 'NodeContext'.
newtype ContextHolder ssc m a = ContextHolder
    { getContextHolder :: ReaderT (NodeContext ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog,
                MonadTxpLD ssc, MonadFix)

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder ctx = flip runReaderT ctx . getContextHolder

instance Monad m => WrappedM (ContextHolder ssc m) where
    type UnwrappedM (ContextHolder ssc m) = ReaderT (NodeContext ssc) m
    _WrappedM = iso getContextHolder ContextHolder

instance MonadBase IO m => MonadBase IO (ContextHolder ssc m) where
    liftBase = lift . liftBase

type instance ThreadId (ContextHolder ssc m) = ThreadId m
type instance Promise (ContextHolder ssc m) = Promise m
type instance SharedAtomicT (ContextHolder ssc m) = SharedAtomicT m
type instance Counter (ContextHolder ssc m) = Counter m
type instance Distribution (ContextHolder ssc m) = Distribution m
type instance SharedExclusiveT (ContextHolder ssc m) = SharedExclusiveT m
type instance Gauge (ContextHolder ssc m) = Gauge m
type instance ChannelT (ContextHolder ssc m) = ChannelT m

deriving instance MonadDB ssc m => MonadDB ssc (ContextHolder ssc m)

instance ( Mockable d m
         , MFunctor' d (ReaderT (NodeContext ssc) m) m
         , MFunctor' d (ContextHolder ssc m) (ReaderT (NodeContext ssc) m)
         ) => Mockable d (ContextHolder ssc m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WithNodeContext ssc (ContextHolder ssc m) where
    getNodeContext = ContextHolder ask

instance (Mockable CurrentTime m, MonadIO m) =>
         MonadSlots (ContextHolder ssc m) where
    getSystemStartTime = ContextHolder $ asks ncSystemStart

    getCurrentTime = do
        lastMargin <- readNtpMargin
        Timestamp . (+ lastMargin) <$> currentTime

    getCurrentSlot = do
        lastSlot <- readNtpLastSlot
        ntpData <- readNtpData
        getCurrentSlotUsingNtp lastSlot ntpData

    getSlotDuration = pure genesisSlotDuration

instance (MonadIO m, Mockable Catch m, WithLogger m) => MonadJL (ContextHolder ssc m) where
    jlLog ev = ContextHolder (asks ncJLFile) >>= maybe (pure ()) doLog
      where
        doLog logFileMV =
          (liftIO . withMVar logFileMV $ flip appendJL ev)
            `catchAll` \e -> logWarning $ sformat ("Can't write to json log: " % shown) e
