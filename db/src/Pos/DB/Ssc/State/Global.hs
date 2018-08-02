{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Methods that operate on 'SscGlobalState' and 'VssCertificatesMap'.

module Pos.DB.Ssc.State.Global
       (
       -- * Certs
         getGlobalCerts
       , getStableCerts

       -- * Global state
       , sscLoadGlobalState
       , sscGetGlobalState
       ) where

import           Formatting (build, sformat, (%))
import           Universum

import           Pos.Chain.Ssc (MonadSscMem, SscGlobalState (..),
                     getStableCertsPure, sgsVssCertificates, sscRunGlobalQuery)
import qualified Pos.Chain.Ssc as Ssc
import           Pos.Core (EpochIndex (..), HasGenesisData,
                     HasProtocolConstants, SlotId (..))
import           Pos.Core.Ssc (VssCertificatesMap (..))
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logInfo)

import           Pos.DB (MonadDBRead)
import qualified Pos.DB.Ssc.GState as DB

----------------------------------------------------------------------------
-- Certs
----------------------------------------------------------------------------

getGlobalCerts
    :: (MonadSscMem ctx m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        Ssc.certs .
        Ssc.setLastKnownSlot sl <$>
        view sgsVssCertificates

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (MonadSscMem ctx m, MonadIO m, HasGenesisData, HasProtocolConstants)
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view sgsVssCertificates)

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

-- | Load global state from DB by recreating it from recent blocks.
sscLoadGlobalState
    :: MonadDBRead m
    => TraceNamed m
    -> m SscGlobalState
sscLoadGlobalState logTrace = do
    logDebug logTrace "Loading SSC global state"
    gs <- DB.getSscGlobalState
    gs <$ logInfo logTrace (sformat ("Loaded SSC state: " %build) gs)

sscGetGlobalState
    :: (MonadSscMem ctx m, MonadIO m)
    => m SscGlobalState
sscGetGlobalState = sscRunGlobalQuery ask
