{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Propagation of runtime configuration.

module Pos.Update.Configuration
       ( UpdateConfiguration(..)
       , HasUpdateConfiguration
       , updateConfiguration
       , withUpdateConfiguration

       , ourAppName
       , ourSystemTag
       , lastKnownBlockVersion
       , curSoftwareVersion
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import           Data.Maybe (fromMaybe)
import           Data.Reflection (Given (..), give)
import qualified Data.Text as T
import           Distribution.System (buildArch, buildOS)
import           Language.Haskell.TH (runIO)
import qualified Language.Haskell.TH.Syntax as TH (lift)
import           Serokell.Util.ANSI  (Color (Red, Blue), colorize)

-- For FromJSON instances.
import           Pos.Aeson.Core ()
import           Pos.Aeson.Update ()
import           Pos.Core (ApplicationName, BlockVersion (..), SoftwareVersion (..))
import           Pos.Core.Update (SystemTag, archHelper, mkSystemTag, osHelper)

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
ourAppName :: HasUpdateConfiguration => ApplicationName
ourAppName = ccApplicationName updateConfiguration

-- | Last block version application is aware of.
lastKnownBlockVersion :: HasUpdateConfiguration => BlockVersion
lastKnownBlockVersion = ccLastKnownBlockVersion updateConfiguration

-- | Version of application (code running)
curSoftwareVersion :: HasUpdateConfiguration => SoftwareVersion
curSoftwareVersion = SoftwareVersion ourAppName (ccApplicationVersion updateConfiguration)

-- | @SystemTag@ corresponding to the operating system/architecture pair the program was
-- compiled in.
-- The @Distribution.System@ module
-- (https://hackage.haskell.org/package/Cabal-2.0.1.1/docs/Distribution-System.html)
-- from @Cabal@ was used to access to a build's host machine @OS@ and @Arch@itecture
-- information.
currentSystemTag :: SystemTag
currentSystemTag =
    $(do let st :: Either Text SystemTag
             st = mkSystemTag (toText (osHelper buildOS ++ archHelper buildArch))
             color c s = "\n" <> colorize c s <> "\n"
         case st of Left e -> error . color Red . T.concat $
                                  ["Current system tag could not be calculated: ", e]
                    Right tag -> do runIO . putStrLn . color Blue . T.concat $
                                        ["Current system tag is: ", show tag]
                                    TH.lift tag
     )

ourSystemTag :: HasUpdateConfiguration => SystemTag
ourSystemTag = ccSystemTag updateConfiguration
