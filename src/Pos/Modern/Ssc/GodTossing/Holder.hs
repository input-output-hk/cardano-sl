{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pos.Modern.Ssc.GodTossing.Holder
       (
         SscHolder (..)
       , runSscHolder
       ) where
import           Serokell.Util.Verify    (VerificationRes)
import           Universum

import qualified Control.Concurrent.STM  as STM
import           Pos.Ssc.Class.Storage   (MonadSscGS (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types.Types         (Address, EpochIndex, SlotLeaders, Utxo)

import           Control.Lens                    (iso)
import           Control.Monad.Base              (MonadBase (..))
import           Control.Monad.Catch             (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader            (ReaderT (ReaderT))
import           Control.Monad.Trans.Class       (MonadTrans)
import           Control.Monad.Trans.Control     (ComposeSt, MonadBaseControl (..),
                                                  MonadTransControl (..), StM,
                                                  defaultLiftBaseWith, defaultLiftWith,
                                                  defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc            (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed          (MonadTimed (..), ThreadId)
import           Data.Default                    (Default, def)
import           Serokell.Util.Lens              (WrappedM (..))
import           System.Wlog                     (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                     (WithNodeContext)
import           Pos.Slotting                    (MonadSlots (..))
import           Pos.Ssc.Class.LocalData         (MonadSscLD (..))
import           Pos.Ssc.Class.Types             (Ssc (SscLocalData))
import           Pos.State                       (MonadDB (..))
import           Pos.Txp.LocalData               (MonadTxLD (..))
import           Pos.Util.JsonLog                (MonadJL (..))

import qualified Pos.Modern.DB                   as Modern
import           Pos.Modern.Txp.Class            (MonadTxpLD (..))
import           Pos.Modern.Txp.Storage.Types    (MemPool, UtxoView)
import qualified Pos.Modern.Txp.Storage.UtxoView as UV
import           Pos.Types                       (HeaderHash, MonadUtxo (..),
                                                  MonadUtxoRead (..))
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
                MonadDB ssc, CanLog, MonadMask)

instance Monad m => WrappedM (SscHolder ssc m) where
    type UnwrappedM (SscHolder ssc m) = ReaderT (SscState ssc) m
    _WrappedM = iso getSscHolder SscHolder

instance MonadTransfer s m => MonadTransfer s (SscHolder ssc m)

type instance ThreadId (SscHolder ssc m) = ThreadId m

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

runSscHolder :: MonadIO m => SscHolder ssc m a -> SscGlobalState ssc -> SscLocalData ssc -> m a
runSscHolder holder glob loc = SscState
                       <$> liftIO (STM.newTVarIO glob)
                       <*> liftIO (STM.newTVarIO loc)
                       >>= runReaderT (getSscHolder holder)
