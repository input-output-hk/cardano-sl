{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Propagation of runtime configuration.

module Pos.Chain.Update.Configuration
       ( UpdateConfiguration(..)
       , HasUpdateConfiguration
       , updateConfiguration
       , withUpdateConfiguration

       , ourAppName
       , ourSystemTag
       , lastKnownBlockVersion
       , curSoftwareVersion

       , currentSystemTag

       , ccApplicationName_L
       , ccLastKnownBlockVersion_L
       , ccApplicationVersion_L
       , ccSystemTag_L
       ) where

import           Universum

import           Control.Lens (makeLensesWith)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject,
                     (.:), (.:?), (.=))
import           Data.Maybe (fromMaybe)
import           Data.Reflection (Given (..), give)
import           Distribution.System (buildArch, buildOS)

import           Pos.Chain.Update.ApplicationName
                     (ApplicationName (ApplicationName))
import           Pos.Chain.Update.BlockVersion (BlockVersion (..))
import           Pos.Chain.Update.SoftwareVersion (SoftwareVersion (..))
import           Pos.Chain.Update.SystemTag (SystemTag (..), archHelper,
                     osHelper)
import           Pos.Util (postfixLFields)

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
    deriving (Eq, Generic, Show)

makeLensesWith postfixLFields ''UpdateConfiguration

instance ToJSON UpdateConfiguration where
    toJSON (UpdateConfiguration (ApplicationName appname) lkbv appver (SystemTag systag)) = object [
        "applicationName" .= appname
      , "lastKnownBlockVersion" .= lkbv
      , "applicationVersion" .= appver
      , "systemTag" .= systag
      ]

instance FromJSON UpdateConfiguration where
    parseJSON = withObject "UpdateConfiguration" $ \o -> do
        ccApplicationName       <- o .: "applicationName"
        ccLastKnownBlockVersion <- o .: "lastKnownBlockVersion"
        ccApplicationVersion    <- o .: "applicationVersion"
        ccSystemTag             <- fromMaybe currentSystemTag <$> o .:? "systemTag"
        pure UpdateConfiguration {..}

----------------------------------------------------------------------------
-- Various constants
----------------------------------------------------------------------------

-- | Name of our application.
ourAppName :: UpdateConfiguration -> ApplicationName
ourAppName = ccApplicationName

-- | Last block version application is aware of.
lastKnownBlockVersion :: UpdateConfiguration -> BlockVersion
lastKnownBlockVersion = ccLastKnownBlockVersion

-- | Version of application (code running)
curSoftwareVersion :: UpdateConfiguration -> SoftwareVersion
curSoftwareVersion uc = SoftwareVersion (ourAppName uc) (ccApplicationVersion uc)

-- | @SystemTag@ corresponding to the operating system/architecture pair the program was
-- compiled in.
-- The @Distribution.System@ module
-- (https://hackage.haskell.org/package/Cabal-2.0.1.1/docs/Distribution-System.html)
-- from @Cabal@ was used to access to a build's host machine @OS@ and @Arch@itecture
-- information.
currentSystemTag :: SystemTag
currentSystemTag = SystemTag (toText (osHelper buildOS ++ archHelper buildArch))

ourSystemTag :: UpdateConfiguration -> SystemTag
ourSystemTag = ccSystemTag
