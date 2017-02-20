{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monad transformer which stores SSC data.

module Pos.Ssc.Extra.Holder
       ( SscHolder (..)
       , mkSscHolderState
       , mkStateAndRunSscHolder
       , runSscHolder
       , ignoreSscHolder
       ) where

import qualified Control.Concurrent.STM    as STM
import           Control.Lens              (iso)
import           Control.Monad.Base        (MonadBase (..))
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Reader      (ReaderT (ReaderT))
import           Control.Monad.Trans.Class (MonadTrans)
import           Mockable                  (ChannelT, Counter, Distribution, Gauge, Gauge,
                                            MFunctor', Mockable (liftMockable), Promise,
                                            SharedAtomicT, SharedExclusiveT,
                                            SharedExclusiveT, ThreadId,
                                            liftMockableWrappedM)
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName, WithLogger)
import           Universum

import           Pos.Context               (WithNodeContext)
import           Pos.DB                    (MonadDB)
import           Pos.DB.Limits             (MonadDBLimits)
import           Pos.Slotting.Class        (MonadSlots, MonadSlotsData)
import           Pos.Ssc.Class.LocalData   (SscLocalDataClass (sscNewLocalData))
import           Pos.Ssc.Class.Storage     (SscGStateClass (sscLoadGlobalState))
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
               , MonadSlotsData
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
               , MonadDBLimits
               )

type instance ThreadId (SscHolder ssc m) = ThreadId m

instance MonadBase IO m => MonadBase IO (SscHolder ssc m) where
    liftBase = lift . liftBase

type instance ThreadId (SscHolder ssc m) = ThreadId m
type instance Promise (SscHolder ssc m) = Promise m
type instance SharedAtomicT (SscHolder ssc m) = SharedAtomicT m
type instance Counter (SscHolder ssc m) = Counter m
type instance Distribution (SscHolder ssc m) = Distribution m
type instance SharedExclusiveT (SscHolder ssc m) = SharedExclusiveT m
type instance Gauge (SscHolder ssc m) = Gauge m
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
       ( --WithLogger m
       --, WithNodeContext ssc m
       --, SscGStateClass ssc
       --, SscLocalDataClass ssc
       --, MonadDB ssc m
       --, MonadSlots m
       )
    => SscState ssc
    -> SscHolder ssc m a
    -> m a
runSscHolder st holder = runReaderT (getSscHolder holder) st

mkStateAndRunSscHolder
    :: forall ssc m a.
       ( WithLogger m
       , WithNodeContext ssc m
       , SscGStateClass ssc
       , SscLocalDataClass ssc
       , MonadDB ssc m
       , MonadSlots m
       )
    => SscHolder ssc m a
    -> m a
mkStateAndRunSscHolder holder = do
    st <- mkSscHolderState
    runSscHolder st holder

mkSscHolderState
    :: forall ssc m .
       ( WithLogger m
       , WithNodeContext ssc m
       , SscGStateClass ssc
       , SscLocalDataClass ssc
       , MonadDB ssc m
       , MonadSlots m
       )
    => m (SscState ssc)
mkSscHolderState = do
    gState <- sscLoadGlobalState @ssc
    ld <- sscNewLocalData @ssc
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld

ignoreSscHolder :: SscHolder ssc m a -> m a
ignoreSscHolder holder =
    runReaderT (getSscHolder holder) (panic "SSC var: don't force me")
