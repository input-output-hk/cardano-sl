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
import           Pos.Core as Core (Config, EpochIndex (..), SlotId (..))
import           Pos.Core.Ssc (VssCertificatesMap (..))
import           Pos.DB (MonadDBRead)
import qualified Pos.DB.Ssc.GState as DB
import           Pos.Util.Wlog (WithLogger, logDebug, logInfo)

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
    :: (MonadSscMem ctx m, MonadIO m)
    => Core.Config
    -> EpochIndex
    -> m VssCertificatesMap
getStableCerts coreConfig epoch = getStableCertsPure coreConfig epoch
    <$> sscRunGlobalQuery (view sgsVssCertificates)

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

-- | Load global state from DB by recreating it from recent blocks.
sscLoadGlobalState :: (MonadDBRead m, WithLogger m) => m SscGlobalState
sscLoadGlobalState = do
    logDebug "Loading SSC global state"
    gs <- DB.getSscGlobalState
    gs <$ logInfo (sformat ("Loaded SSC state: " %build) gs)

sscGetGlobalState
    :: (MonadSscMem ctx m, MonadIO m)
    => m SscGlobalState
sscGetGlobalState = sscRunGlobalQuery ask
