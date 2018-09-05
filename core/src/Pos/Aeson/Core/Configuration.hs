{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Derived FromJSON instance for Pos.Core.Configuration.Configuration

module Pos.Aeson.Core.Configuration
    (
    ) where

import           Data.Aeson.TH (deriveJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Core.Configuration.Core (CoreConfiguration (..))

deriveJSON defaultOptions ''CoreConfiguration
