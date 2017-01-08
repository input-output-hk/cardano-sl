{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monad transformer which stores SSC data.

module Pos.Ssc.Extra.Holder
       ( SscHolder (..)
       , SscState
       , runSscHolder
       , runSscHolderRaw
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (ReaderT))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Data.Default                (Default (def))
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import qualified Pos.DB                      as Modern (MonadDB (..))
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass)
import           Pos.Ssc.Class.Types         (Ssc (..))
import           Pos.Ssc.Extra.MonadGS       (MonadSscGS (..))
import           Pos.Ssc.Extra.MonadLD       (MonadSscLD (..))
import           Pos.Ssc.Extra.Richmen       (MonadSscRichmen (..))
import           Pos.Types                   (EpochIndex, RichmenStake,
                                              readUntilEpochMVar)
import           Pos.Util.JsonLog            (MonadJL (..))

data SscState ssc =
    SscState
    {
      sscGlobal  :: !(STM.TVar (SscGlobalState ssc))
    , sscLocal   :: !(STM.TVar (SscLocalData ssc))
    , sscRichmen :: !(MVar (EpochIndex, RichmenStake))
    }

newtype SscHolder ssc m a =
    SscHolder
    { getSscHolder :: ReaderT (SscState ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, MonadDialog s p, WithNodeContext ssc,
                MonadJL, CanLog, MonadMask, Modern.MonadDB ssc)

instance MonadTransfer s m => MonadTransfer s (SscHolder ssc m)
type instance ThreadId (SscHolder ssc m) = ThreadId m

instance MonadBase IO m => MonadBase IO (SscHolder ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (SscHolder ssc) where
    type StT (SscHolder ssc) a = StT (ReaderT (SscState ssc)) a
    liftWith = defaultLiftWith SscHolder getSscHolder
    restoreT = defaultRestoreT SscHolder

instance MonadBaseControl IO m => MonadBaseControl IO (SscHolder ssc m) where
    type StM (SscHolder ssc m) a = ComposeSt (SscHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance Monad m => WrappedM (SscHolder ssc m) where
    type UnwrappedM (SscHolder ssc m) = ReaderT (SscState ssc) m
    _WrappedM = iso getSscHolder SscHolder

instance MonadIO m => MonadSscGS ssc (SscHolder ssc m) where
    getGlobalState = SscHolder (asks sscGlobal) >>= atomically . STM.readTVar
    modifyGlobalState f = SscHolder ask >>= \sscSt -> atomically $ do
                g <- STM.readTVar (sscGlobal sscSt)
                let (res, ng) = f g
                STM.writeTVar (sscGlobal sscSt) ng
                return res
    setGlobalState newSt = SscHolder (asks sscGlobal) >>= atomically . flip STM.writeTVar newSt

instance MonadIO m => MonadSscLD ssc (SscHolder ssc m) where
    getLocalData = SscHolder (asks sscLocal) >>= atomically . STM.readTVar
    modifyLocalData f = SscHolder ask >>= \sscSt -> atomically $ do
                g <- STM.readTVar (sscGlobal sscSt)
                l <- STM.readTVar (sscLocal sscSt)
                let (res, nl) = f (g, l)
                STM.writeTVar (sscLocal sscSt) nl
                return res
    setLocalData newSt = SscHolder (asks sscLocal) >>= atomically . flip STM.writeTVar newSt

instance MonadIO m => MonadSscRichmen (SscHolder ssc m) where
    -- | Put richmen into MVar.
    writeSscRichmen er = do
        mvar <- SscHolder (asks sscRichmen)
        _ <- liftIO $ tryTakeMVar mvar
        liftIO $ putMVar mvar er

    -- | Read richmen from SSC node context corresponding to epoch.
    -- This function blocks if richmen are not available
    -- or they was computed for previous epoch
    readSscRichmen epoch = do
        mvar <- SscHolder (asks sscRichmen)
        snd <$> readUntilEpochMVar mvar epoch

    tryReadSscRichmen = SscHolder (asks sscRichmen) >>= liftIO . tryReadMVar

runSscHolder :: forall ssc m a. (SscLocalDataClass ssc, MonadIO m)
             => SscHolder ssc m a -> SscGlobalState ssc -> m a
runSscHolder holder glob = SscState
                       <$> liftIO (STM.newTVarIO glob)
                       <*> liftIO (STM.newTVarIO def)
                       <*> liftIO newEmptyMVar
                       >>= runReaderT (getSscHolder holder)

runSscHolderRaw :: SscState ssc -> SscHolder ssc m a -> m a
runSscHolderRaw st holder = runReaderT (getSscHolder holder) st
