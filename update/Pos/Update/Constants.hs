module Pos.Update.Constants
       ( UpdateConstants(..)
       , updateConstants

       , ourAppName
       , lastKnownBlockVersion
       , curSoftwareVersion
       , ourSystemTag

       -- * Genesis constants
       , genesisBlockVersion
       , genesisSoftwareVersions
       , genesisAppNames
       ) where

import           Universum                  hiding (lift)

import           Data.Aeson                 (FromJSON (..), genericParseJSON)
import qualified Data.Aeson.Types           as A
import           Data.Tagged                (Tagged (..))
import           Language.Haskell.TH.Syntax (lift, runIO)
import           Serokell.Aeson.Options     (defaultOptions)
import           System.Environment         (lookupEnv)

import           Pos.Core                   (ApplicationName, BlockVersion (..),
                                             SoftwareVersion (..), mkApplicationName)
import           Pos.Update.Core            (SystemTag, mkSystemTag)
import           Pos.Util.Config            (IsConfig (..), configParser,
                                             parseFromCslConfig)

----------------------------------------------------------------------------
-- Config itself
----------------------------------------------------------------------------

updateConstants :: UpdateConstants
updateConstants =
    case parseFromCslConfig configParser of
        Left err -> error (toText ("Couldn't parse update config: " ++ err))
        Right x  -> x

data UpdateConstants = UpdateConstants
    {
      -- | Name of this application.
      ccApplicationName    :: !Text
      -- | Last known block version: major
    , ccLastKnownBVMajor   :: !Word16
      -- | Last known block version: minor
    , ccLastKnownBVMinor   :: !Word16
      -- | Last known block version: alt
    , ccLastKnownBVAlt     :: !Word8
      -- | Application version
    , ccApplicationVersion :: !Word32
    }
    deriving (Show, Generic)

instance FromJSON UpdateConstants where
    parseJSON = checkConstants <=< genericParseJSON defaultOptions

instance IsConfig UpdateConstants where
    configPrefix = Tagged Nothing

-- | Check invariants
checkConstants :: UpdateConstants -> A.Parser UpdateConstants
checkConstants cs@UpdateConstants{..} = do
    unless (isJust $ mkApplicationName ccApplicationName) $
        fail "UpdateConstants: mkApplicationName ccApplicationName is Nothing"
    pure cs

----------------------------------------------------------------------------
-- Various constants
----------------------------------------------------------------------------

-- | Name of our application.
ourAppName :: ApplicationName
ourAppName =
    either (error . mappend "Failed to init our application name: ") identity $
    mkApplicationName $ ccApplicationName updateConstants

-- | Last block version application is aware of.
lastKnownBlockVersion :: BlockVersion
lastKnownBlockVersion = BlockVersion (ccLastKnownBVMajor updateConstants)
                                     (ccLastKnownBVMinor updateConstants)
                                     (ccLastKnownBVAlt updateConstants)

-- | Version of application (code running)
curSoftwareVersion :: SoftwareVersion
curSoftwareVersion = SoftwareVersion ourAppName
                                     (ccApplicationVersion updateConstants)

ourSystemTag :: SystemTag
ourSystemTag =
    $(do mbTag <- runIO (lookupEnv "CSL_SYSTEM_TAG")
         case mbTag of
             Just tag -> lift =<< mkSystemTag (toText tag)
             Nothing ->
                 fail $
                 "Failed to init ourSystemTag: \
                       \couldn't find env var \"CSL_SYSTEM_TAG\". " <>
                 "If you have no idea what it means, just set " <>
                 "\"CSL_SYSTEM_TAG\" environmental variable (to arbitrary value)")

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
    f (nameStr, Left err) =
        error $
        "Failed to create ApplicationName for " <> nameStr <> ": " <> err
    f (_, Right appName) = SoftwareVersion {svAppName = appName, svNumber = 0}

genesisAppNames :: [(Text, Either Text ApplicationName)]
genesisAppNames = map f ["cardano-sl", "csl-daedalus"]
  where
    f name = (name, mkApplicationName name)
