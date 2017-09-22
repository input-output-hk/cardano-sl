-- | Configuration for running tests

module Test.Pos.Configuration (testConf) where

import           Universum

import qualified Data.Aeson                 as J
import           Pos.Launcher.Configuration (Configuration (..))
import           Pos.Util.Config            (embedYamlConfigCT)

testConf :: Configuration
testConf = case J.fromJSON $ J.Object jobj of
              J.Error str    -> error $ toText str
              J.Success conf -> conf
  where
    jobj = $(embedYamlConfigCT (Proxy @J.Object) "configuration.yaml" "configuration.yaml" "test")
