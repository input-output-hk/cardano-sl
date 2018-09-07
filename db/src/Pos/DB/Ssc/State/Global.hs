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

import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.Chain.Ssc (MonadSscMem, SscGlobalState (..),
                     VssCertificatesMap (..), getStableCertsPure,
                     sgsVssCertificates, sscRunGlobalQuery)
import qualified Pos.Chain.Ssc as Ssc
import           Pos.Core (EpochIndex (..), SlotId (..))
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
    => Genesis.Config
    -> EpochIndex
    -> m VssCertificatesMap
getStableCerts genesisConfig epoch = getStableCertsPure genesisConfig epoch
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
