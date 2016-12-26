{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Monad transformer which stores SSC data.

module Pos.Ssc.Extra.Holder
       ( SscHolder (..)
       , runSscHolder
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
import           Pos.Ssc.Extra.MonadLD       (MonadSscLD (..), MonadSscLDM (..))
import           Pos.Util.JsonLog            (MonadJL (..))

data SscState ssc =
    SscState
    {
      sscGlobal :: !(STM.TVar (SscGlobalState ssc))
    , sscLocal  :: !(STM.TVar (SscLocalData ssc))
    }

newtype SscHolder ssc m a =
    SscHolder
    { getSscHolder :: ReaderT (SscState ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                CanLog, MonadMask, Modern.MonadDB ssc)

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

instance MonadIO m => MonadSscLDM ssc (SscHolder ssc m) where
    getLocalDataM = SscHolder (asks sscLocal) >>= atomically . STM.readTVar
    modifyLocalDataM f = SscHolder ask >>= \sscSt -> atomically $ do
                g <- STM.readTVar (sscGlobal sscSt)
                l <- STM.readTVar (sscLocal sscSt)
                let (res, nl) = f (g, l)
                STM.writeTVar (sscLocal sscSt) nl
                return res
    setLocalDataM newSt = SscHolder (asks sscLocal) >>= atomically . flip STM.writeTVar newSt

instance MonadSscLD ssc m => MonadSscLD ssc (SscHolder ssc m) where
    getLocalData = lift  getLocalData
    setLocalData = lift . setLocalData

runSscHolder :: forall ssc m a. (SscLocalDataClass ssc, MonadIO m)
             => SscHolder ssc m a -> SscGlobalState ssc -> m a
runSscHolder holder glob = SscState
                       <$> liftIO (STM.newTVarIO glob)
                       <*> liftIO (STM.newTVarIO def)
                       >>= runReaderT (getSscHolder holder)
