{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TossT monad transformer. Single-threaded.

module Pos.Ssc.GodTossing.Toss.Trans
       ( TossT (..)
       , runTossT
       , evalTossT
       , execTossT
       ) where

import           Control.Lens                  (at, iso, (.=))
import           Control.Monad.Base            (MonadBase (..))
import           Control.Monad.Except          (MonadError)
import           Control.Monad.Fix             (MonadFix)
import           Control.Monad.State           (MonadState (..))
import           Control.Monad.Trans.Class     (MonadTrans)
import           Control.Monad.Trans.Control   (ComposeSt, MonadBaseControl (..),
                                                MonadTransControl (..), StM,
                                                defaultLiftBaseWith, defaultLiftWith,
                                                defaultRestoreM, defaultRestoreT)
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import           Mockable                      (ChannelT, Promise, SharedAtomicT,
                                                ThreadId)
import           Serokell.Util.Lens            (WrappedM (..))
import           System.Wlog                   (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                   (WithNodeContext)
import           Pos.Crypto                    (hash)
import           Pos.DB.Class                  (MonadDB)
import           Pos.Delegation.Class          (MonadDelegation)
import           Pos.Slotting                  (MonadSlots (..))
import           Pos.Ssc.Extra                 (MonadSscMem)
import           Pos.Ssc.GodTossing.Toss.Class (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Types (TossModifier (..), tmCertificates,
                                                tmCommitments, tmOpenings, tmShares)
import           Pos.Types                     (SoftwareVersion (..))
import           Pos.Util.JsonLog              (MonadJL (..))

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores TossModifier and implements
-- writable MonadToss.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
newtype TossT m a = TossT
    { getTossT :: StateT TossModifier m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadThrow
               , MonadSlots
               , MonadCatch
               , MonadIO
               , HasLoggerName
               , MonadTrans
               , MonadError e
               , WithNodeContext ssc
               , MonadJL
               , CanLog
               , MonadMask
               , MonadSscMem mem
               , MonadBase io
               )

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runTossT :: TossModifier -> TossT m a -> m (a, TossModifier)
runTossT m (TossT s) = runStateT s m

evalTossT :: Functor m => TossModifier -> TossT m a -> m a
evalTossT m = fmap fst . runTossT m

execTossT :: Functor m => TossModifier -> TossT m a -> m TossModifier
execTossT m = fmap snd . runTossT m

----------------------------------------------------------------------------
-- MonadToss
----------------------------------------------------------------------------

-- instance MonadTossRead m =>
--          MonadTossRead (TossT m) where

-- instance MonadTossRead m =>
--          MonadToss (TossT m) where

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

deriving instance MonadDB ssc m => MonadDB ssc (TossT m)
type instance ThreadId (TossT m) = ThreadId m
type instance Promise (TossT m) = Promise m
type instance SharedAtomicT (TossT m) = SharedAtomicT m
type instance ChannelT (TossT m) = ChannelT m

instance Monad m => WrappedM (TossT m) where
    type UnwrappedM (TossT m) = StateT TossModifier m
    _WrappedM = iso getTossT TossT

instance MonadTransControl TossT where
    type StT (TossT) a = StT (StateT TossModifier) a
    liftWith = defaultLiftWith TossT getTossT
    restoreT = defaultRestoreT TossT

instance MonadBaseControl IO m => MonadBaseControl IO (TossT m) where
    type StM (TossT m) a = ComposeSt TossT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM
