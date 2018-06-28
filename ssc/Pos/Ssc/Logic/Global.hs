{-# LANGUAGE Rank2Types #-}

-- | Thin wrapper around `calculateSeed` used in LRC worker.

module Pos.Ssc.Logic.Global
       (
       -- * Seed
         sscCalculateSeed
       ) where

import           Universum

import           Pos.Binary.Ssc ()
import           Pos.Core (EpochIndex (..), SharedSeed, VssCertificatesMap (..),
                     vcVssKey)
import           Pos.DB (MonadDBRead)
import           Pos.Lrc.Consumer.Ssc (getSscRichmen)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Lrc.Types (RichmenStakes)
import           Pos.Ssc.Error (SscSeedError)
import           Pos.Ssc.Mem (MonadSscMem, SscGlobalQuery, sscRunGlobalQuery)
import           Pos.Ssc.Seed (calculateSeed)
import           Pos.Ssc.Types (sgsCommitments, sgsOpenings, sgsShares,
                     sgsVssCertificates)
import qualified Pos.Ssc.VssCertData as VCD
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem)

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
    => Trace m LogItem
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
    <*> (map vcVssKey . getVssCertificatesMap . VCD.certs <$>
         view sgsVssCertificates)
    <*> view sgsOpenings
    <*> view sgsShares
    <*> pure richmen
