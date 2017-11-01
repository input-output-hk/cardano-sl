{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Working with global SSC state.

module Pos.Ssc.GState
       (
       -- * Certs
         getGlobalCerts
       , getStableCerts

       -- * Seed
       , sscCalculateSeed

       -- * Global state
       , sscLoadGlobalState
       , sscGetGlobalState

       -- * Blocks
       , module Pos.Ssc.GState.BlockLogic
       ) where

import           Formatting                       (build, sformat, (%))
import           System.Wlog                      (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc                   ()
import           Pos.Core                         (EpochIndex (..),
                                                   HasConfiguration,
                                                   SharedSeed, SlotId (..),
                                                   VssCertificatesMap (..),
                                                   vcVssKey)
import           Pos.DB                           (MonadDBRead)
import           Pos.Lrc.Context                  (HasLrcContext)
import           Pos.Lrc.Types                    (RichmenStakes)
import           Pos.Ssc.Configuration            (HasSscConfiguration)
import qualified Pos.Ssc.DB                       as DB
import           Pos.Ssc.Functions                (getStableCertsPure)
import           Pos.Ssc.Seed                     (calculateSeed)
import qualified Pos.Ssc.VssCertData              as VCD
import           Pos.Ssc.Lrc                      (getSscRichmenFromLrc)
import           Pos.Ssc.Mem                      (MonadSscMem, SscGlobalQuery,
                                                   sscRunGlobalQuery)
import           Pos.Ssc.Error                    (SscSeedError)
import           Pos.Ssc.Types                    (SscGlobalState (..),
                                                   sgsCommitments, sgsOpenings, sgsShares,
                                                   sgsVssCertificates)

-- Reexports
import           Pos.Ssc.GState.BlockLogic

----------------------------------------------------------------------------
-- Certs
----------------------------------------------------------------------------

getGlobalCerts
    :: (MonadSscMem ctx m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view sgsVssCertificates

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (HasSscConfiguration, HasConfiguration, MonadSscMem ctx m, MonadIO m)
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view sgsVssCertificates)

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

-- | Calculate 'SharedSeed' for given epoch using 'SscGlobalState'.
sscCalculateSeed
    :: forall ctx m.
       ( MonadSscMem ctx m
       , MonadDBRead m
       , MonadReader ctx m
       , HasLrcContext ctx
       , MonadIO m
       , WithLogger m )
    => EpochIndex
    -> m (Either SscSeedError SharedSeed)
sscCalculateSeed epoch = do
    -- We take richmen for the previous epoch because during N-th epoch we
    -- were using richmen for N-th epoch for everything – so, when we are
    -- calculating the seed for N+1-th epoch, we should still use data from
    -- N-th epoch.
    richmen <- getSscRichmenFromLrc "sscCalculateSeed" (epoch - 1)
    sscRunGlobalQuery $ sscCalculateSeedQ epoch richmen

sscCalculateSeedQ
    :: EpochIndex
    -> RichmenStakes
    -> SscGlobalQuery (Either SscSeedError SharedSeed)
sscCalculateSeedQ _epoch richmen =
    calculateSeed
    <$> view sgsCommitments
    <*> (map vcVssKey . getVssCertificatesMap . VCD.certs <$>
         view sgsVssCertificates)
    <*> view sgsOpenings
    <*> view sgsShares
    <*> pure richmen

----------------------------------------------------------------------------
-- Global state
----------------------------------------------------------------------------

-- | Load global state from DB by recreating it from recent blocks.
sscLoadGlobalState :: (HasConfiguration, MonadDBRead m, WithLogger m) => m SscGlobalState
sscLoadGlobalState = do
    logDebug "Loading SSC global state"
    gs <- DB.getSscGlobalState
    gs <$ logInfo (sformat ("Loaded GodTossing state: " %build) gs)

sscGetGlobalState
    :: (MonadSscMem ctx m, MonadIO m)
    => m SscGlobalState
sscGetGlobalState = sscRunGlobalQuery ask
