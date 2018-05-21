{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | PollT monad transformer. Single-threaded.

module Pos.Update.Poll.Trans
       ( PollT
       , runPollT
       , evalPollT
       , execPollT
       ) where

import           Universum

import           Control.Lens (uses, (%=), (.=))
import           Control.Monad.State (MonadState (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Ether
import           System.Wlog (logWarning)

import           Pos.Binary.Update ()
import           Pos.Core (SoftwareVersion (..), addressHash)
import           Pos.Core.Update (UpdateProposal (..))
import           Pos.Crypto (hash)
import           Pos.Update.BlockVersion (applyBVM)
import           Pos.Update.Poll.Class (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Modifier (PollModifier (..), pmActivePropsL, pmAdoptedBVFullL,
                                           pmBVsL, pmConfirmedL, pmConfirmedPropsL,
                                           pmEpochProposersL, pmSlottingDataL)
import           Pos.Update.Poll.Types (BlockVersionState (..), DecidedProposalState (..),
                                        ProposalState (..), UndecidedProposalState (..),
                                        bvsIsConfirmed, cpsSoftwareVersion, psProposal)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (ether)

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores PollModifier and implements
-- writable MonadPoll.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
type PollT = Ether.LazyStateT' PollModifier

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runPollT :: PollModifier -> PollT m a -> m (a, PollModifier)
runPollT = flip Ether.runLazyStateT

evalPollT :: Monad m => PollModifier -> PollT m a -> m a
evalPollT = flip Ether.evalLazyStateT

execPollT :: Monad m => PollModifier -> PollT m a -> m PollModifier
execPollT = flip Ether.execLazyStateT

----------------------------------------------------------------------------
-- MonadPoll
----------------------------------------------------------------------------

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
        new <- pmSlottingData <$> get
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
