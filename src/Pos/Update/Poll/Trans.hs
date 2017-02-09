{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PollT monad transformer. Single-threaded.

module Pos.Update.Poll.Trans
       ( PollT (..)
       , runPollT
       , evalPollT
       , execPollT
       ) where

import           Control.Lens                (at, iso, (.=))
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.State         (MonadState (..))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              Gauge, Promise, SharedAtomicT,
                                              SharedExclusiveT, SharedExclusiveT,
                                              ThreadId)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.Crypto                  (hash)
import           Pos.DB.Class                (MonadDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Slotting.Class          (MonadSlots, MonadSlotsData)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types                   (SoftwareVersion (..))
import           Pos.Types.Utxo.Class        (MonadUtxo, MonadUtxoRead)
import           Pos.Update.Core             (UpdateProposal (..))
import           Pos.Update.MemState.Class   (MonadUSMem (..))
import           Pos.Update.Poll.Class       (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types       (BlockVersionState (..), PollModifier (..),
                                              cpsSoftwareVersion, pmAdoptedBVFullL,
                                              pmDelActivePropsIdxL, pmDelActivePropsL,
                                              pmDelBVsL, pmDelConfirmedL,
                                              pmDelConfirmedPropsL, pmNewActivePropsIdxL,
                                              pmNewActivePropsL, pmNewBVsL,
                                              pmNewConfirmedL, pmNewConfirmedPropsL,
                                              pmSlottingDataL, psProposal)
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
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadSlotsData, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadTrans, MonadError e,
                WithNodeContext ssc, MonadJL, CanLog, MonadMask, MonadUSMem,
                MonadSscMem mem, MonadUtxoRead, MonadUtxo,
                MonadTxpLD ssc, MonadBase io, MonadDelegation, MonadFix)

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runPollT :: PollModifier -> PollT m a -> m (a, PollModifier)
runPollT m (PollT s) = runStateT s m

evalPollT :: Functor m => PollModifier -> PollT m a -> m a
evalPollT m = fmap fst . runPollT m

execPollT :: Functor m => PollModifier -> PollT m a -> m PollModifier
execPollT m = fmap snd . runPollT m

----------------------------------------------------------------------------
-- MonadPoll
----------------------------------------------------------------------------

instance MonadPollRead m =>
         MonadPollRead (PollT m) where
    getBVState pv = do
        new <- pmNewBVs <$> PollT get
        maybe (PollT $ getBVState pv) (pure . Just) $ HM.lookup pv new
    getProposedBVs = PollT getProposedBVs
    getAdoptedBVFull = PollT $ do
        new <- pmAdoptedBVFull <$> get
        maybe getAdoptedBVFull pure new
    getLastConfirmedSV appName = do
        new <- pmNewConfirmed <$> PollT get
        maybe (PollT $ getLastConfirmedSV appName) (pure . Just) $
            HM.lookup appName new
    hasActiveProposal appName = do
        new <- pmNewActivePropsIdx <$> PollT get
        del <- pmDelActivePropsIdx <$> PollT get
        if | appName `HM.member` del -> return False
           | appName `HM.member` new -> return True
           | otherwise -> PollT $ hasActiveProposal appName
    getProposal upId = do
        new <- pmNewActiveProps <$> PollT get
        del <- pmDelActiveProps <$> PollT get
        if | upId `HS.member` del -> return Nothing
           | Just res <- HM.lookup upId new -> return (Just res)
           | otherwise -> PollT $ getProposal upId
    getSlottingData = PollT $ do
        new <- pmSlottingData <$> get
        maybe getSlottingData pure new

instance MonadPollRead m =>
         MonadPoll (PollT m) where
    putBVState bv st = PollT $ pmNewBVsL . at bv .= Just st
    delBVState bv = PollT $ pmDelBVsL . at bv .= Nothing
    setAdoptedBV bv = do
        bvs <- getBVState bv
        case bvs of
            Nothing               -> pass -- can't happen actually
            Just (bvsData -> bvd) -> PollT $ pmAdoptedBVFullL .= Just (bv, bvd)
    setLastConfirmedSV SoftwareVersion {..} =
        PollT $ pmNewConfirmedL . at svAppName .= Just svNumber
    delConfirmedSV appName = PollT $ pmDelConfirmedL . at appName .= Nothing
    addConfirmedProposal cps =
        PollT $ pmNewConfirmedPropsL . at (cpsSoftwareVersion cps) .= Just cps
    delConfirmedProposal sv = PollT $ pmDelConfirmedPropsL . at sv .= Nothing
    addActiveProposal ps =
        PollT $ do
            let up = psProposal ps
                upId = hash up
                sv = upSoftwareVersion up
                appName = svAppName sv
            pmNewActivePropsL . at upId .= Just ps
            pmNewActivePropsIdxL . at appName .= Just upId
            pmDelActivePropsL . at upId .= Nothing
            pmDelActivePropsIdxL . at appName .= Nothing
    deactivateProposal id =
        PollT $ do
            prop <- getProposal id
            whenJust prop $ \ps -> do
                let up = psProposal ps
                    sv = upSoftwareVersion up
                    appName = svAppName sv
                pmNewActivePropsL . at id .= Nothing
                pmNewActivePropsIdxL . at appName .= Nothing
                pmDelActivePropsL . at id .= Just ()
                pmDelActivePropsIdxL . at appName .= Just id
    setSlottingData sd = PollT $ pmSlottingDataL .= Just sd

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

deriving instance MonadDB ssc m => MonadDB ssc (PollT m)
type instance ThreadId (PollT m) = ThreadId m
type instance Promise (PollT m) = Promise m
type instance SharedAtomicT (PollT m) = SharedAtomicT m
type instance Counter (PollT m) = Counter m
type instance Distribution (PollT m) = Distribution m
type instance SharedExclusiveT (PollT m) = SharedExclusiveT m
type instance Gauge (PollT m) = Gauge m
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
