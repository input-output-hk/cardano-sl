{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Txp.Txp.DBTxp
    ( DBTxp (..)
    ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans         (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultRestoreM)
import qualified Data.HashMap.Strict         as HM
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor', Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName, WithLogger)
import           Universum

import           Pos.Context                 (WithNodeContext, lrcActionOnEpochReason)
import           Pos.DB.Class                (MonadDB)
import qualified Pos.DB.GState               as GS
import           Pos.DB.Lrc                  (getIssuersStakes, getRichmenUS)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Lrc.Types               (FullRichmenData)
import           Pos.Slotting.Class          (MonadSlots, MonadSlotsData)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.Types                   (Coin)
import           Pos.Update.MemState.Class   (MonadUSMem (..))
import           Pos.Update.Poll.Class       (MonadPollRead (..))
import           Pos.Util.JsonLog            (MonadJL (..))

newtype DBTxp m a = DBTxp
    { runDBTxp :: m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadThrow
               , MonadSlotsData
               , MonadSlots
               , MonadCatch
               , MonadIO
               , MonadFail
               , HasLoggerName
               , MonadError e
               , WithNodeContext ssc
               , MonadJL
               , CanLog
               , MonadMask
               , MonadUSMem
               , MonadSscMem peka
               , MonadBase io
               , MonadDelegation
               , MonadFix
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

deriving instance MonadDB ssc m => MonadDB ssc (DBTxp m)
type instance ThreadId (DBTxp m) = ThreadId m
type instance Promise (DBTxp m) = Promise m
type instance SharedAtomicT (DBTxp m) = SharedAtomicT m
type instance Counter (DBTxp m) = Counter m
type instance Distribution (DBTxp m) = Distribution m
type instance SharedExclusiveT (DBTxp m) = SharedExclusiveT m
type instance Gauge (DBTxp m) = Gauge m
type instance ChannelT (DBTxp m) = ChannelT m

instance MonadTrans DBTxp where
    lift = DBTxp

instance ( Mockable d m
         , MFunctor' d (DBTxp m) m
         ) => Mockable d (DBTxp m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (DBTxp m) where
    type UnwrappedM (DBTxp m) = m
    _WrappedM = iso runDBTxp DBTxp

instance MonadTransControl DBTxp where
    type StT DBTxp a = a
    liftWith f = DBTxp $ f $ runDBTxp
    restoreT = DBTxp

instance MonadBaseControl IO m => MonadBaseControl IO (DBTxp m) where
    type StM (DBTxp m) a = ComposeSt DBTxp m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
