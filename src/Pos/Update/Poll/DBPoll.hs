{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instance of MoandPollRead which uses DB.

module Pos.Update.Poll.DBPoll
       ( DBPoll (..)
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
import           Mockable                    (ChannelT, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import qualified Pos.DB.GState               as GS
import           Pos.DB.Lrc                  (getRichmenUS)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Lrc.Types               (FullRichmenData)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Extra               (MonadSscGS (..), MonadSscLD (..))
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types.Types             (Coin)
import           Pos.Types.Utxo.Class        (MonadUtxo, MonadUtxoRead)
import           Pos.Update.MemState.Class   (MonadUSMem (..))
import           Pos.Update.Poll.Class       (MonadPollRead (..))
import           Pos.Util.JsonLog            (MonadJL (..))

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

newtype DBPoll m a = DBPoll
    { runDBPoll :: m a
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, MonadFail, HasLoggerName, MonadError e,
                WithNodeContext ssc, MonadJL, CanLog, MonadMask, MonadUSMem,
                MonadSscLD kek, MonadSscGS ssc, MonadUtxoRead, MonadUtxo,
                MonadTxpLD ssc, MonadBase io, MonadDelegation, MonadFix)

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

deriving instance MonadDB ssc m => MonadDB ssc (DBPoll m)
type instance ThreadId (DBPoll m) = ThreadId m
type instance Promise (DBPoll m) = Promise m
type instance SharedAtomicT (DBPoll m) = SharedAtomicT m
type instance ChannelT (DBPoll m) = ChannelT m

instance MonadTrans DBPoll where
    lift = DBPoll

instance ( Mockable d m
         , MFunctor' d (DBPoll m) m
         ) => Mockable d (DBPoll m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (DBPoll m) where
    type UnwrappedM (DBPoll m) = m
    _WrappedM = iso runDBPoll DBPoll

instance MonadTransControl DBPoll where
    type StT DBPoll a = a
    liftWith f = DBPoll $ f $ runDBPoll
    restoreT = DBPoll

instance MonadBaseControl IO m => MonadBaseControl IO (DBPoll m) where
    type StM (DBPoll m) a = ComposeSt DBPoll m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

----------------------------------------------------------------------------
-- MonadPoll
----------------------------------------------------------------------------

instance MonadDB patak m =>
         MonadPollRead (DBPoll m) where
    getScriptVersion = GS.getScriptVersion
    getLastAdoptedPV = GS.getLastPV
    getLastConfirmedSV = GS.getConfirmedSV
    hasActiveProposal = fmap isJust . GS.getAppProposal
    getProposal = GS.getProposalState
    getEpochTotalStake e = fmap fst <$> getRichmenUS e
    getRichmanStake e id = (findStake =<<) <$> getRichmenUS e
      where
        findStake :: FullRichmenData -> Maybe Coin
        findStake = HM.lookup id . snd
    getOldProposals = GS.getOldProposals
