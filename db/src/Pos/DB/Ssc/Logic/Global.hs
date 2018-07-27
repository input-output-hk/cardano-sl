{-# LANGUAGE Rank2Types #-}

-- | Thin wrapper around `calculateSeed` used in LRC worker.

module Pos.DB.Ssc.Logic.Global
       (
       -- * Seed
         sscCalculateSeed
       ) where

import           Universum

import           Pos.Chain.Lrc (RichmenStakes)
import           Pos.Core (EpochIndex (..), SharedSeed)
import           Pos.Core.Ssc (VssCertificatesMap (..), vcVssKey)
import           Pos.DB (MonadDBRead)
import           Pos.DB.Lrc (HasLrcContext, getSscRichmen)
import           Pos.Ssc.Error (SscSeedError)
import           Pos.Ssc.Mem (MonadSscMem, SscGlobalQuery, sscRunGlobalQuery)
import           Pos.Ssc.Seed (calculateSeed)
import           Pos.Ssc.Types (sgsCommitments, sgsOpenings, sgsShares,
                     sgsVssCertificates)
import qualified Pos.Ssc.VssCertData as VCD

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

-- | Calculate 'SharedSeed' for given epoch using 'SscGlobalState'.
sscCalculateSeed
    :: forall ctx m.
       ( MonadSscMem ctx m
       , MonadDBRead m
       , HasLrcContext ctx
       , MonadIO m
       )
    => EpochIndex
    -> m (Either SscSeedError SharedSeed)
sscCalculateSeed epoch = do
    -- We take richmen for the previous epoch because during N-th epoch we
    -- were using richmen for N-th epoch for everything – so, when we are
    -- calculating the seed for N+1-th epoch, we should still use data from
    -- N-th epoch.
    richmen <- getSscRichmen "sscCalculateSeed" (epoch - 1)
    sscRunGlobalQuery $ sscCalculateSeedQ richmen

sscCalculateSeedQ
    :: RichmenStakes
    -> SscGlobalQuery (Either SscSeedError SharedSeed)
sscCalculateSeedQ richmen =
    calculateSeed
    <$> view sgsCommitments
    <*> (map vcVssKey . getVssCertificatesMap . VCD.certs <$>
         view sgsVssCertificates)
    <*> view sgsOpenings
    <*> view sgsShares
    <*> pure richmen
