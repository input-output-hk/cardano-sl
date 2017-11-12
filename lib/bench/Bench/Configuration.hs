{-# LANGUAGE Rank2Types #-}

module Bench.Configuration (benchConf, giveCoreConf) where

import           Universum

import qualified Data.Aeson as J
import           Pos.Core (HasConfiguration, withGenesisSpec)
import           Pos.Launcher.Configuration (Configuration (..))
import           Pos.Util.Config (embedYamlConfigCT)

benchConf :: Configuration
benchConf = case J.fromJSON $ J.Object jobj of
              J.Error str    -> error $ toText str
              J.Success conf -> conf
  where
    jobj = $(embedYamlConfigCT (Proxy @J.Object) "configuration.yaml" "configuration.yaml" "test")

giveCoreConf :: (HasConfiguration => r) -> r
giveCoreConf = withGenesisSpec 0 (ccCore benchConf)
