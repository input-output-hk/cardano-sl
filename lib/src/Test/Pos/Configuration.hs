-- | Default configuration for running tests.

module Test.Pos.Configuration
       ( defaultTestConf
       , defaultTestGenesisSpec
       , defaultTestBlockVersionData
       ) where

import           Universum

import qualified Data.Aeson as J

import           Pos.Core (BlockVersionData)
import           Pos.Core.Configuration (CoreConfiguration (..), GenesisConfiguration (..))
import           Pos.Core.Genesis (GenesisSpec (..))
import           Pos.Launcher.Configuration (Configuration (..))
import           Pos.Util.Config (embedYamlConfigCT)

-- | This configuration is embedded into binary and is used by default
-- in tests.
defaultTestConf :: Configuration
defaultTestConf = case J.fromJSON $ J.Object jobj of
              J.Error str    -> error $ toText str
              J.Success conf -> conf
  where
    jobj = $(embedYamlConfigCT (Proxy @J.Object) "configuration.yaml" "configuration.yaml" "test")

defaultTestGenesisSpec :: GenesisSpec
defaultTestGenesisSpec =
    case ccGenesis (ccCore defaultTestConf) of
        GCSpec spec -> spec
        _           -> error "unexpected genesis type in test"

defaultTestBlockVersionData :: BlockVersionData
defaultTestBlockVersionData = gsBlockVersionData defaultTestGenesisSpec
