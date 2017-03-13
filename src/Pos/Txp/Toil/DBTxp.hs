{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instances of MoandUtxoRead and MonadBalancesRead which use DB.

module Pos.Txp.Toil.DBTxp
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
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor', Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import qualified Pos.DB.GState               as GS
import qualified Pos.DB.GState.Balances      as GS
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.Update.MemState.Class   (MonadUSMem (..))
import           Pos.Util.JsonLog            (MonadJL (..))

import           Pos.Txp.Toil.Class          (MonadBalancesRead (..), MonadUtxoRead (..))
#ifdef WITH_EXPLORER
import           Pos.Txp.Toil.Class          (MonadTxExtraRead (..))
#endif

newtype DBTxp m a = DBTxp
    { runDBTxp :: m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadThrow
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
               , MonadDB
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Useful instances
----------------------------------------------------------------------------

instance (Monad m, MonadDB m) => MonadUtxoRead (DBTxp m) where
    utxoGet = GS.getTxOut

instance (Monad m, MonadDB m) => MonadBalancesRead (DBTxp m) where
    getTotalStake = GS.getTotalFtsStake
    getStake = GS.getFtsStake

#ifdef WITH_EXPLORER
instance (Monad m, MonadDB m) => MonadTxExtraRead (DBTxp m) where
    getTxExtra = GS.getTxExtra
#endif
