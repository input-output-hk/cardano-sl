{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monad transformer which stores SSC data.

module Pos.Ssc.Extra.Holder
       ( SscHolder (..)
       , runSscHolder
       , runSscHolderRaw
       ) where

import qualified Control.Concurrent.STM    as STM
import           Control.Lens              (iso)
import           Control.Monad.Base        (MonadBase (..))
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Reader      (ReaderT (ReaderT))
import           Control.Monad.Trans.Class (MonadTrans)
import           Mockable                  (ChannelT, MFunctor', Mockable (liftMockable),
                                            Promise, SharedAtomicT, ThreadId,
                                            liftMockableWrappedM)
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName, WithLogger)
import           Universum

import           Pos.Context               (WithNodeContext)
import           Pos.DB                    (MonadDB (..))
import           Pos.Slotting              (MonadSlots (..))
import           Pos.Ssc.Class.LocalData   (SscLocalDataClass (sscNewLocalData))
import           Pos.Ssc.Class.Storage     (SscStorageClass (sscLoadGlobalState))
import           Pos.Ssc.Extra.Class       (MonadSscMem (..))
import           Pos.Ssc.Extra.Types       (SscState (..))
import           Pos.Util.JsonLog          (MonadJL (..))

newtype SscHolder ssc m a = SscHolder
    { getSscHolder :: ReaderT (SscState ssc) m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadThrow
               , MonadSlots
               , MonadCatch
               , MonadIO
               , MonadFail
               , HasLoggerName
               , WithNodeContext ssc
               , MonadJL
               , CanLog
               , MonadMask
               , MonadFix
               )

type instance ThreadId (SscHolder ssc m) = ThreadId m

instance MonadBase IO m => MonadBase IO (SscHolder ssc m) where
    liftBase = lift . liftBase

type instance ThreadId (SscHolder ssc m) = ThreadId m
type instance Promise (SscHolder ssc m) = Promise m
type instance SharedAtomicT (SscHolder ssc m) = SharedAtomicT m
type instance ChannelT (SscHolder ssc m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (SscState ssc) m) m
         , MFunctor' d (SscHolder ssc m) (ReaderT (SscState ssc) m)
         ) => Mockable d (SscHolder ssc m) where
    liftMockable = liftMockableWrappedM

deriving instance MonadDB ssc m => MonadDB ssc (SscHolder ssc m)

instance Monad m => WrappedM (SscHolder ssc m) where
    type UnwrappedM (SscHolder ssc m) = ReaderT (SscState ssc) m
    _WrappedM = iso getSscHolder SscHolder

instance Monad m => MonadSscMem ssc (SscHolder ssc m) where
    askSscMem = SscHolder ask

-- | Run 'SscHolder' reading GState from DB (restoring from blocks)
-- and using default (uninitialized) local state.
runSscHolder
    :: forall ssc m a.
       ( WithLogger m
       , SscStorageClass ssc
       , SscLocalDataClass ssc
       , MonadDB ssc m
       , MonadSlots m
       )
    => SscHolder ssc m a -> m a
runSscHolder holder = do
    gState <- sscLoadGlobalState @ssc
    ld <- sscNewLocalData @ssc
    sscState <- liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
    runReaderT (getSscHolder holder) sscState

runSscHolderRaw :: SscState ssc -> SscHolder ssc m a -> m a
runSscHolderRaw st holder = runReaderT (getSscHolder holder) st
