{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PollT monad transformer. Single-threaded.

module Pos.Update.Poll.Trans
       ( PollT (..)
       , runPollT
       , evalPollT
       , execPollT
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.State         (MonadState (..))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Data.Default                (Default (def))
import           Mockable                    (ChannelT, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Extra               (MonadSscGS (..), MonadSscLD (..))
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types.Utxo.Class        (MonadUtxo, MonadUtxoRead)
import           Pos.Update.MemState.Class   (MonadUSMem (..))
import           Pos.Update.MemState.Types   (PollModifier)
import           Pos.Update.Poll.Class       (MonadPoll (..), MonadPollRead (..))
import           Pos.Util.JsonLog            (MonadJL (..))

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores PollModifier and implements
-- writable MonadPoll.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
newtype PollT m a = PollT
    { getPollT :: StateT PollModifier m a
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadTrans,
                WithNodeContext ssc, MonadJL, CanLog, MonadMask, MonadUSMem,
                MonadSscLD kek, MonadSscGS ssc, MonadUtxoRead, MonadUtxo,
                MonadTxpLD ssc, MonadBase io, MonadDelegation, MonadFix)

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runPollT :: Monad m => PollModifier -> PollT m a -> m (a, PollModifier)
runPollT m (PollT s) = runStateT s m

evalPollT :: Monad m => PollModifier -> PollT m a -> m a
evalPollT m = fmap fst . runPollT m

execPollT :: Monad m => PollModifier -> PollT m a -> m PollModifier
execPollT m = fmap snd . runPollT m

----------------------------------------------------------------------------
-- MonadPoll
----------------------------------------------------------------------------

instance MonadPollRead m =>
         MonadPollRead (PollT m) where
    getScriptVersion = notImplemented
    getLastAdoptedPV = notImplemented
    getLastConfirmedSV = notImplemented
    hasActiveProposal = notImplemented
    getProposal = notImplemented

instance MonadPollRead m =>
         MonadPoll (PollT m) where
    addScriptVersionDep = notImplemented
    setLastAdoptedPV = notImplemented
    setLastConfirmedSV = notImplemented
    addActiveProposal = notImplemented

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

deriving instance MonadDB ssc m => MonadDB ssc (PollT m)
type instance ThreadId (PollT m) = ThreadId m
type instance Promise (PollT m) = Promise m
type instance SharedAtomicT (PollT m) = SharedAtomicT m
type instance ChannelT (PollT m) = ChannelT m

-- instance ( Mockable d m
--          , MFunctor' d (PollT m) (StateT PollModifier m)
--          , MFunctor' d (StateT PollModifier m) m
--          ) => Mockable d (PollT m) where
--     liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (PollT m) where
    type UnwrappedM (PollT m) = StateT PollModifier m
    _WrappedM = iso getPollT PollT

instance MonadTransControl PollT where
    type StT (PollT) a = StT (StateT PollModifier) a
    liftWith = defaultLiftWith PollT getPollT
    restoreT = defaultRestoreT PollT

instance MonadBaseControl IO m => MonadBaseControl IO (PollT m) where
    type StM (PollT m) a = ComposeSt PollT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM
