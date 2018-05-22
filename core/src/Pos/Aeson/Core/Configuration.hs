{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Derived FromJSON instance for Pos.Core.Configuration.Configuration

module Pos.Aeson.Core.Configuration
    (
    ) where

import           Data.Aeson.TH (deriveJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Aeson.Genesis ()
import           Pos.Core.Configuration.Core (CoreConfiguration (..), GenesisConfiguration (..))

deriveJSON defaultOptions ''GenesisConfiguration
deriveJSON defaultOptions ''CoreConfiguration
