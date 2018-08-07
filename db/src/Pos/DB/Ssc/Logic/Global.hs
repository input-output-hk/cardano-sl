{-# LANGUAGE Rank2Types #-}

-- | Thin wrapper around `calculateSeed` used in LRC worker.

module Pos.DB.Ssc.Logic.Global
       (
       -- * Seed
         sscCalculateSeed
       ) where

import           Universum

import           Pos.Chain.Lrc (RichmenStakes)
import           Pos.Chain.Ssc (MonadSscMem, SscGlobalQuery, SscSeedError,
                     calculateSeed, sgsCommitments, sgsOpenings, sgsShares,
                     sgsVssCertificates, sscRunGlobalQuery)
import qualified Pos.Chain.Ssc as Ssc
import           Pos.Core (EpochIndex (..), SharedSeed)
import           Pos.Core.Ssc (VssCertificatesMap (..), vcVssKey)
import           Pos.DB (MonadDBRead)
import           Pos.DB.Lrc (HasLrcContext, getSscRichmen)
import           Pos.Util.Trace.Named (TraceNamed)

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
    => TraceNamed m
    -> EpochIndex
    -> m (Either SscSeedError SharedSeed)
sscCalculateSeed logTrace epoch = do
    -- We take richmen for the previous epoch because during N-th epoch we
    -- were using richmen for N-th epoch for everything – so, when we are
    -- calculating the seed for N+1-th epoch, we should still use data from
    -- N-th epoch.
    richmen <- getSscRichmen "sscCalculateSeed" (epoch - 1)
    sscRunGlobalQuery $ sscCalculateSeedQ richmen logTrace

sscCalculateSeedQ
    :: RichmenStakes
    -> SscGlobalQuery (Either SscSeedError SharedSeed)
sscCalculateSeedQ richmen _ =
    calculateSeed
    <$> view sgsCommitments
    <*> (map vcVssKey . getVssCertificatesMap . Ssc.certs <$>
         view sgsVssCertificates)
    <*> view sgsOpenings
    <*> view sgsShares
    <*> pure richmen
