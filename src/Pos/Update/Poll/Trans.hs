{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PollT monad transformer. Single-threaded.

module Pos.Update.Poll.Trans
       ( PollT (..)
       , runPollT
       , evalPollT
       , execPollT
       ) where

import           Control.Lens                (iso, uses, (%=), (.=))
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
import           System.Wlog                 (CanLog, HasLoggerName, logWarning)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.Core                    (addressHash)
import           Pos.Crypto                  (hash)
import           Pos.DB.Class                (MonadDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Slotting.Class          (MonadSlots)
import           Pos.Slotting.MemState       (MonadSlotsData)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.Types                   (SoftwareVersion (..))
import           Pos.Update.Core             (UpdateProposal (..))
import           Pos.Update.Poll.Class       (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Types       (BlockVersionState (..),
                                              DecidedProposalState (..),
                                              PollModifier (..), ProposalState (..),
                                              UndecidedProposalState (..),
                                              cpsSoftwareVersion, pmActivePropsL,
                                              pmAdoptedBVFullL, pmBVsL, pmConfirmedL,
                                              pmConfirmedPropsL, pmEpochProposersL,
                                              pmSlottingDataL, psProposal)
import           Pos.Util.Context            (MonadContext (..))
import           Pos.Util.JsonLog            (MonadJL (..))
import qualified Pos.Util.Modifier           as MM

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
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadThrow
               , MonadSlotsData
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
               , MonadDB
               , MonadBase io
               , MonadDelegation
               , MonadFix
               )

instance MonadContext m => MonadContext (PollT m) where
    type ContextType (PollT m) = ContextType m

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
    getBVState pv = PollT $ MM.lookupM getBVState pv =<< use pmBVsL
    getProposedBVs = PollT $ MM.keysM getProposedBVs =<< use pmBVsL
    getEpochProposers = PollT $ do
        new <- use pmEpochProposersL
        maybe getEpochProposers pure new
    getConfirmedBVStates =
        PollT $
        filter (bvsIsConfirmed . snd) <$>
        (MM.toListM getConfirmedBVStates =<< use pmBVsL)
    getAdoptedBVFull =
        PollT $ maybe getAdoptedBVFull pure =<< use pmAdoptedBVFullL
    getLastConfirmedSV appName =
        PollT $ MM.lookupM getLastConfirmedSV appName =<< use pmConfirmedL
    getProposal upId =
        PollT $ MM.lookupM getProposal upId =<< use pmActivePropsL
    getProposalsByApp app = PollT $ do
        let eqApp = (== app) . svAppName . upSoftwareVersion . psProposal . snd
        props <- uses pmActivePropsL (filter eqApp . MM.insertions)
        dbProps <- map (first (hash . psProposal) . join (,)) <$> getProposalsByApp app
        pure . toList . HM.fromList $ dbProps ++ props -- squash props with same upId
    getConfirmedProposals =
        PollT $
        MM.valuesM
            (map (first cpsSoftwareVersion . join (,)) <$> getConfirmedProposals) =<<
        use pmConfirmedPropsL
    getEpochTotalStake = lift . getEpochTotalStake
    getRichmanStake e = lift . getRichmanStake e
    getOldProposals sl =
        PollT $
        map snd <$>
        (MM.mapMaybeM getOldProposalPairs extractOld =<< use pmActivePropsL)
      where
        extractOld (PSUndecided ups)
            | upsSlot ups <= sl = Just ups
            | otherwise = Nothing
        extractOld (PSDecided _) = Nothing
        getOldProposalPairs =
            map (\ups -> (hash $ upsProposal ups, ups)) <$> getOldProposals sl
    getDeepProposals cd =
        PollT $
        map snd <$>
        (MM.mapMaybeM getDeepProposalPairs extractDeep =<< use pmActivePropsL)
      where
        extractDeep (PSDecided dps)
            | Just propDifficulty <- dpsDifficulty dps
            , propDifficulty <= cd = Just dps
            | otherwise = Nothing
        extractDeep (PSUndecided _) = Nothing
        getDeepProposalPairs =
            map (\dps -> (hash $ upsProposal $ dpsUndecided dps, dps)) <$>
            getDeepProposals cd
    getBlockIssuerStake e = lift . getBlockIssuerStake e
    getSlottingData =
        PollT $ do
            new <- pmSlottingData <$> get
            maybe getSlottingData pure new

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

instance MonadPollRead m =>
         MonadPoll (PollT m) where
    putBVState bv st = PollT $ pmBVsL %= MM.insert bv st
    delBVState bv = PollT $ pmBVsL %= MM.delete bv
    setAdoptedBV bv = do
        bvs <- getBVState bv
        case bvs of
            Nothing ->
                logWarning $ "setAdoptedBV: unknown version " <> pretty bv -- can't happen actually
            Just (bvsData -> bvd) -> PollT $ pmAdoptedBVFullL .= Just (bv, bvd)
    setLastConfirmedSV SoftwareVersion {..} =
        PollT $ pmConfirmedL %= MM.insert svAppName svNumber
    delConfirmedSV appName = PollT $ pmConfirmedL %= MM.delete appName
    addConfirmedProposal cps =
        PollT $ pmConfirmedPropsL %= MM.insert (cpsSoftwareVersion cps) cps
    delConfirmedProposal sv = PollT $ pmConfirmedPropsL %= MM.delete sv
    insertActiveProposal ps = do
        let up@UnsafeUpdateProposal{..} = psProposal ps
            upId = hash up
        whenNothingM_ (getProposal upId) $
            setEpochProposers =<< (HS.insert (addressHash upFrom) <$> getEpochProposers)
        PollT $ pmActivePropsL %= MM.insert upId ps
    -- Deactivate proposal doesn't change epoch proposers.
    deactivateProposal id = do
        prop <- getProposal id
        whenJust prop $ \ps ->
            PollT $ do
                let up = psProposal ps
                    upId = hash up
                pmActivePropsL %= MM.delete upId
    setSlottingData sd = PollT $ pmSlottingDataL .= Just sd
    setEpochProposers ep = PollT $ pmEpochProposersL .= Just ep

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (PollT m) = ThreadId m
type instance Promise (PollT m) = Promise m
type instance SharedAtomicT (PollT m) = SharedAtomicT m
type instance Counter (PollT m) = Counter m
type instance Distribution (PollT m) = Distribution m
type instance SharedExclusiveT (PollT m) = SharedExclusiveT m
type instance Gauge (PollT m) = Gauge m
type instance ChannelT (PollT m) = ChannelT m

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
