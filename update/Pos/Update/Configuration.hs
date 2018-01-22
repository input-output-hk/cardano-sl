{-# LANGUAGE Rank2Types #-}

-- | Propagation of runtime configuration.

module Pos.Update.Configuration
       ( UpdateConfiguration(..)
       , HasUpdateConfiguration
       , updateConfiguration
       , withUpdateConfiguration

       , ourAppName
       , lastKnownBlockVersion
       , curSoftwareVersion
       , ourSystemTag
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), genericParseJSON)
import           Data.Reflection (Given (..), give)
import           Serokell.Aeson.Options (defaultOptions)

-- For FromJSON instances.
import           Pos.Aeson.Core ()
import           Pos.Aeson.Update ()
import           Pos.Core (ApplicationName, BlockVersion (..), SoftwareVersion (..))
import           Pos.Core.Update (SystemTag)

----------------------------------------------------------------------------
-- Config itself
----------------------------------------------------------------------------

type HasUpdateConfiguration = Given UpdateConfiguration

withUpdateConfiguration :: UpdateConfiguration -> (HasUpdateConfiguration => r) -> r
withUpdateConfiguration = give

updateConfiguration :: HasUpdateConfiguration => UpdateConfiguration
updateConfiguration = given

data UpdateConfiguration = UpdateConfiguration
    {
      -- | Name of this application.
      ccApplicationName       :: !ApplicationName
      -- | Last known block version
    , ccLastKnownBlockVersion :: !BlockVersion
      -- | Application version
    , ccApplicationVersion    :: !Word32
      -- | System tag.
    , ccSystemTag             :: !SystemTag
    }
    deriving (Show, Generic)

instance FromJSON UpdateConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Various constants
----------------------------------------------------------------------------

-- | Name of our application.
ourAppName :: HasUpdateConfiguration => ApplicationName
ourAppName = ccApplicationName updateConfiguration

-- | Last block version application is aware of.
lastKnownBlockVersion :: HasUpdateConfiguration => BlockVersion
lastKnownBlockVersion = ccLastKnownBlockVersion updateConfiguration

-- | Version of application (code running)
curSoftwareVersion :: HasUpdateConfiguration => SoftwareVersion
curSoftwareVersion = SoftwareVersion ourAppName (ccApplicationVersion updateConfiguration)

ourSystemTag :: HasUpdateConfiguration => SystemTag
ourSystemTag = ccSystemTag updateConfiguration
