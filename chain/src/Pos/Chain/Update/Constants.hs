module Pos.Chain.Update.Constants
       ( -- * Genesis constants
         genesisBlockVersion
       , genesisSoftwareVersions
       , genesisAppNames
       ) where

import           Universum

import           Pos.Chain.Update.ApplicationName (ApplicationName (..))
import           Pos.Chain.Update.BlockVersion (BlockVersion (..))
import           Pos.Chain.Update.SoftwareVersion (SoftwareVersion (..))

----------------------------------------------------------------------------
-- Genesis constants
----------------------------------------------------------------------------

-- | BlockVersion used at the very beginning.
genesisBlockVersion :: BlockVersion
genesisBlockVersion =
    BlockVersion
    { bvMajor = 0
    , bvMinor = 0
    , bvAlt = 0
    }

-- | Software Versions
genesisSoftwareVersions :: [SoftwareVersion]
genesisSoftwareVersions = map f genesisAppNames
  where
    f (_, appName) = SoftwareVersion {svAppName = appName, svNumber = 0}

genesisAppNames :: [(Text, ApplicationName)]
genesisAppNames = map f ["cardano-sl", "csl-daedalus"]
  where
    f name = (name, ApplicationName name)
