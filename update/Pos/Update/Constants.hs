module Pos.Update.Constants
       ( -- * Genesis constants
         genesisBlockVersion
       , genesisSoftwareVersions
       , genesisAppNames
       ) where

import           Universum

import           Pos.Core (ApplicationName (..), BlockVersion (..), SoftwareVersion (..))

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
    f name = (name, UncheckedApplicationName name)
