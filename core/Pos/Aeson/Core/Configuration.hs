-- | Derived FromJSON instance for Pos.Core.Configuration.Configuration

module Pos.Aeson.Core.Configuration
    (
    ) where

import           Data.Aeson.TH (deriveFromJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Aeson.Genesis ()
import           Pos.Core.Configuration.Core (CoreConfiguration (..), GenesisConfiguration (..))

deriveFromJSON defaultOptions ''GenesisConfiguration
deriveFromJSON defaultOptions ''CoreConfiguration
