{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Type classes for Poll abstraction.

module Pos.Chain.Update.Poll.Class
       ( MonadPollRead (..)
       , MonadPoll (..)

       -- Roll Transformer
       , RollT
       , runRollT
       , execRollT

       -- Poll Transformer
       , PollT
       , runPollT
       , evalPollT
       , execPollT
       ) where

import           Universum hiding (id)

import           Control.Lens (uses, (%=), (.=))
import           Control.Monad.Trans (MonadTrans)
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List (find)
import qualified Ether
import           System.Wlog (WithLogger, logWarning)

import           Pos.Chain.Update.BlockVersion (applyBVM)
import           Pos.Chain.Update.Poll.Modifier (PollModifier (..),
                     pmActivePropsL, pmAdoptedBVFullL, pmBVsL, pmConfirmedL,
                     pmConfirmedPropsL, pmEpochProposersL, pmSlottingDataL)
import           Pos.Chain.Update.Poll.Types (BlockVersionState,
                     BlockVersionState (..), ConfirmedProposalState,
                     DecidedProposalState (..), PrevValue, ProposalState (..),
                     USUndo (..), UndecidedProposalState (..), bvsIsConfirmed,
                     cpsSoftwareVersion, maybeToPrev, psProposal, unChangedBVL,
                     unChangedConfPropsL, unChangedPropsL, unChangedSVL,
                     unLastAdoptedBVL, unPrevProposersL, unSlottingDataL)
import           Pos.Core (ChainDifficulty, Coin, EpochIndex, SlotId,
                     StakeholderId, addressHash)
import           Pos.Core.Slotting (SlottingData)
import           Pos.Core.Update (ApplicationName, BlockVersion,
                     BlockVersionData, NumSoftwareVersion,
                     SoftwareVersion (..), UpId, UpdateProposal (..))
import           Pos.Crypto (hash)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (ether)

----------------------------------------------------------------------------
-- Read-only
----------------------------------------------------------------------------

-- | Type class which provides function necessary for read-only
-- verification of US data.
class (Monad m, WithLogger m) => MonadPollRead m where
    getBVState :: BlockVersion -> m (Maybe BlockVersionState)
    -- ^ Retrieve state of given block version.
    getProposedBVs :: m [BlockVersion]
    -- ^ Retrieve all proposed block versions.
    getEpochProposers :: m (HashSet StakeholderId)
    -- ^ Retrieve all stakeholders who proposed proposals in the current epoch.
    getCompetingBVStates :: m [(BlockVersion, BlockVersionState)]
    -- ^ Get all competing 'BlockVersion's and their states.
    getAdoptedBVFull :: m (BlockVersion, BlockVersionData)
    -- ^ Retrieve last adopted block version and its state.
    getLastConfirmedSV :: ApplicationName -> m (Maybe NumSoftwareVersion)
    -- ^ Get numeric component of last confirmed version of application
    getProposal :: UpId -> m (Maybe ProposalState)
    -- ^ Get active proposal
    getProposalsByApp :: ApplicationName -> m [ProposalState]
    -- ^ Get active proposals for the specified application.
    getConfirmedProposals :: m [ConfirmedProposalState]
    -- ^ Get all known confirmed proposals.
    getEpochTotalStake :: EpochIndex -> m (Maybe Coin)
    -- ^ Get total stake from distribution corresponding to given epoch
    getRichmanStake :: EpochIndex -> StakeholderId -> m (Maybe Coin)
    -- ^ Get stake of ricmhan corresponding to given epoch (if she is
    -- really rich)
    getOldProposals :: SlotId -> m [UndecidedProposalState]
    -- ^ Get all proposals which are in undecided state and were
    -- included into block with slot less than or equal to given.
    getDeepProposals :: ChainDifficulty -> m [DecidedProposalState]
    -- ^ Get all proposals which are in decided state and become
    -- decided deeper than given 'ChainDifficulty'.
    getBlockIssuerStake :: EpochIndex -> StakeholderId -> m (Maybe Coin)
    -- ^ Get stake of issuer of one of the blocks created so far using
    -- stake distribution which is stable in given epoch.
    -- Only issuer of stable block can be passed to this function, otherwise
    -- 'Nothing' will be returned.
    getSlottingData :: m SlottingData
    -- ^ Get most recent 'SlottingData'.

    getAdoptedBV :: m BlockVersion
    getAdoptedBV = fst <$> getAdoptedBVFull

    getAdoptedBVData :: m BlockVersionData
    getAdoptedBVData = snd <$> getAdoptedBVFull

instance {-# OVERLAPPABLE #-}
    (MonadPollRead m, MonadTrans t, Monad (t m), WithLogger (t m)) =>
        MonadPollRead (t m)
  where
    getBVState = lift . getBVState
    getProposedBVs = lift getProposedBVs
    getEpochProposers = lift getEpochProposers
    getCompetingBVStates = lift getCompetingBVStates
    getAdoptedBVFull = lift getAdoptedBVFull
    getLastConfirmedSV = lift . getLastConfirmedSV
    getProposal = lift . getProposal
    getProposalsByApp = lift . getProposalsByApp
    getConfirmedProposals = lift getConfirmedProposals
    getEpochTotalStake = lift . getEpochTotalStake
    getRichmanStake e = lift . getRichmanStake e
    getOldProposals = lift . getOldProposals
    getDeepProposals = lift . getDeepProposals
    getBlockIssuerStake e = lift . getBlockIssuerStake e
    getSlottingData = lift getSlottingData


----------------------------------------------------------------------------
-- Writeable
----------------------------------------------------------------------------

-- | Type class which provides function necessary for verification of
-- US data with ability to modify state.
class MonadPollRead m => MonadPoll m where
    putBVState :: BlockVersion -> BlockVersionState -> m ()
    -- ^ Put state of BlockVersion overriding if it exists.
    delBVState :: BlockVersion -> m ()
    -- ^ Delete BlockVersion and associated state.
    setAdoptedBV :: BlockVersion -> m ()
    -- ^ Set last adopted block version. State is taken from competing states.
    setLastConfirmedSV :: SoftwareVersion -> m ()
    -- ^ Set last confirmed version of application.
    delConfirmedSV :: ApplicationName -> m ()
    -- ^ Del last confirmed version of application.
    addConfirmedProposal :: ConfirmedProposalState -> m ()
    -- ^ Add new confirmed update proposal.
    delConfirmedProposal :: SoftwareVersion -> m ()
    -- ^ Del confirmed update proposal (for rollback only).
    insertActiveProposal :: ProposalState -> m ()
    -- ^ Add new active proposal with its state.
    deactivateProposal :: UpId -> m ()
    -- ^ Delete active proposal given its name and identifier.
    setSlottingData :: SlottingData -> m ()
    -- ^ Set most recent 'SlottingData'.
    setEpochProposers :: HashSet StakeholderId -> m ()
    -- ^ Set proposers.

instance {-# OVERLAPPABLE #-}
    (MonadPoll m, MonadTrans t, Monad (t m), WithLogger (t m)) =>
        MonadPoll (t m)
  where
    putBVState pv = lift . putBVState pv
    delBVState = lift . delBVState
    setAdoptedBV = lift . setAdoptedBV
    setLastConfirmedSV = lift . setLastConfirmedSV
    delConfirmedSV = lift . delConfirmedSV
    addConfirmedProposal = lift . addConfirmedProposal
    delConfirmedProposal = lift . delConfirmedProposal
    insertActiveProposal = lift . insertActiveProposal
    deactivateProposal = lift . deactivateProposal
    setSlottingData = lift . setSlottingData
    setEpochProposers = lift . setEpochProposers

----------------------------------------------------------------------------
-- Roll Transformer
----------------------------------------------------------------------------

type RollT m = Ether.LazyStateT' USUndo m

-- | Monad transformer which stores USUndo and implements writable
-- MonadPoll. Its purpose is to collect data necessary for rollback.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
instance (MonadPoll m) => MonadPoll (RollT m) where
    putBVState bv sv = ether $ do
        insertIfNotExist bv unChangedBVL getBVState
        putBVState bv sv

    delBVState bv = ether $ do
        insertIfNotExist bv unChangedBVL getBVState
        delBVState bv

    setAdoptedBV = setValueWrapper unLastAdoptedBVL getAdoptedBV setAdoptedBV

    setLastConfirmedSV sv@SoftwareVersion{..} = ether $ do
        insertIfNotExist svAppName unChangedSVL getLastConfirmedSV
        setLastConfirmedSV sv

    -- can't be called during apply
    delConfirmedSV = lift . delConfirmedSV

    addConfirmedProposal cps = ether $ do
        confProps <- getConfirmedProposals
        insertIfNotExist (cpsSoftwareVersion cps) unChangedConfPropsL (getter confProps)
        addConfirmedProposal cps
      where
        getter confs sv = pure $ List.find (\x -> cpsSoftwareVersion x == sv) confs

    -- can't be called during apply
    delConfirmedProposal = lift . delConfirmedProposal

    insertActiveProposal ps = ether $ do
        whenNothingM_ (use unPrevProposersL) $ do
            prev <- getEpochProposers
            unPrevProposersL .= Just prev
        insertIfNotExist (hash $ psProposal $ ps) unChangedPropsL getProposal
        insertActiveProposal ps

    deactivateProposal id = ether $ do
        -- Proposer still can't propose new updates in the current epoch
        -- even if his update was deactivated in the same epoch
        insertIfNotExist id unChangedPropsL getProposal
        deactivateProposal id

    setSlottingData =
        setValueWrapper unSlottingDataL getSlottingData setSlottingData
    setEpochProposers =
        setValueWrapper unPrevProposersL getEpochProposers setEpochProposers

-- This is a convenient wrapper for functions which should set some
-- value and this change should be recorded in USUndo. If change of
-- such kind is already recorded in 'USUndo', then we don't record it
-- and just propagate the new value to the underlying 'MonadPoll'. If
-- it is not recorded, we put old value into 'USUndo' before
-- propagating the new value.
setValueWrapper ::
       MonadPoll m
    => Lens' USUndo (Maybe a)
    -> m a
    -> (a -> m ())
    -> a
    -> RollT m ()
setValueWrapper lens getAction setAction value = ether $ do
    whenNothingM_ (use lens) $ do
        prev <- lift getAction
        lens .= Just prev
    lift (setAction value)

insertIfNotExist
    :: (Eq a, Hashable a, MonadState USUndo m)
    => a
    -> Lens' USUndo (HashMap a (PrevValue b))
    -> (a -> m (Maybe b))
    -> m ()
insertIfNotExist id setter getter = do
    whenNothingM_ (HM.lookup id <$> use setter) $ do
        prev <- getter id
        setter %= HM.insert id (maybeToPrev prev)

runRollT :: RollT m a -> m (a, USUndo)
runRollT = flip Ether.runLazyStateT def

execRollT :: Monad m => RollT m a -> m USUndo
execRollT = flip Ether.execLazyStateT def

----------------------------------------------------------------------------
-- PollT Transformer
----------------------------------------------------------------------------

-- | Monad transformer which stores PollModifier and implements
-- writable MonadPoll.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
type PollT = Ether.LazyStateT' PollModifier

runPollT :: PollModifier -> PollT m a -> m (a, PollModifier)
runPollT = flip Ether.runLazyStateT

evalPollT :: Monad m => PollModifier -> PollT m a -> m a
evalPollT = flip Ether.evalLazyStateT

execPollT :: Monad m => PollModifier -> PollT m a -> m PollModifier
execPollT = flip Ether.execLazyStateT

instance (MonadPollRead m) =>
         MonadPollRead (PollT m) where
    getBVState pv = ether $
        MM.lookupM getBVState pv =<< use pmBVsL
    getProposedBVs = ether $
        MM.keysM getProposedBVs =<< use pmBVsL
    getEpochProposers = ether $ do
        new <- use pmEpochProposersL
        maybe getEpochProposers pure new
    getCompetingBVStates = ether $
        filter (bvsIsConfirmed . snd) <$>
        (MM.toListM getCompetingBVStates =<< use pmBVsL)
    getAdoptedBVFull = ether $
        maybe getAdoptedBVFull pure =<< use pmAdoptedBVFullL
    getLastConfirmedSV appName = ether $
        MM.lookupM getLastConfirmedSV appName =<< use pmConfirmedL
    getProposal upId = ether $
        MM.lookupM getProposal upId =<< use pmActivePropsL
    getProposalsByApp app = ether $ do
        let eqApp = (== app) . svAppName . upSoftwareVersion . psProposal . snd
        props <- uses pmActivePropsL (filter eqApp . MM.insertions)
        dbProps <- map (first (hash . psProposal) . join (,)) <$> getProposalsByApp app
        pure . toList . HM.fromList $ dbProps ++ props -- squash props with same upId
    getConfirmedProposals = ether $
        MM.valuesM
            (map (first cpsSoftwareVersion . join (,)) <$> getConfirmedProposals) =<<
        use pmConfirmedPropsL
    getEpochTotalStake = lift . getEpochTotalStake
    getRichmanStake e = lift . getRichmanStake e
    getOldProposals sl = ether $
        map snd <$>
        (MM.mapMaybeM getOldProposalPairs extractOld =<< use pmActivePropsL)
      where
        extractOld (PSUndecided ups)
            | upsSlot ups <= sl = Just ups
            | otherwise = Nothing
        extractOld (PSDecided _) = Nothing
        getOldProposalPairs =
            map (\ups -> (hash $ upsProposal ups, ups)) <$> getOldProposals sl
    getDeepProposals cd = ether $
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
    getSlottingData = ether $ do
        new <- gets pmSlottingData
        maybe getSlottingData pure new

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

instance (MonadPollRead m) =>
         MonadPoll (PollT m) where
    putBVState bv st = ether $ pmBVsL %= MM.insert bv st
    delBVState bv = ether $ pmBVsL %= MM.delete bv
    setAdoptedBV bv = ether $ do
        bvs <- getBVState bv
        adoptedBVD <- getAdoptedBVData
        case bvs of
            Nothing ->
                logWarning $ "setAdoptedBV: unknown version " <> pretty bv -- can't happen actually
            Just (bvsModifier -> bvm) ->
                pmAdoptedBVFullL .= Just (bv, applyBVM bvm adoptedBVD)
    setLastConfirmedSV SoftwareVersion {..} = ether $
        pmConfirmedL %= MM.insert svAppName svNumber
    delConfirmedSV appName = ether $
        pmConfirmedL %= MM.delete appName
    addConfirmedProposal cps = ether $
        pmConfirmedPropsL %= MM.insert (cpsSoftwareVersion cps) cps
    delConfirmedProposal sv = ether $
        pmConfirmedPropsL %= MM.delete sv
    insertActiveProposal ps = do
        let up@UnsafeUpdateProposal{..} = psProposal ps
            upId = hash up
        whenNothingM_ (getProposal upId) $
            setEpochProposers =<< (HS.insert (addressHash upFrom) <$> getEpochProposers)
        ether $ pmActivePropsL %= MM.insert upId ps
    -- Deactivate proposal doesn't change epoch proposers.
    deactivateProposal id = do
        prop <- getProposal id
        whenJust prop $ \ps -> ether $ do
            let up = psProposal ps
                upId = hash up
            pmActivePropsL %= MM.delete upId
    setSlottingData sd = ether $ pmSlottingDataL .= Just sd
    setEpochProposers ep = ether $ pmEpochProposersL .= Just ep
