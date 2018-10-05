{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Derived FromJSON instance for Pos.Core.Configuration.Configuration

module Pos.Aeson.Core.Configuration
    (
    ) where

import           Universum

import           Data.Aeson (FromJSON, parseJSON, withObject, (.!=), (.:), (.:?))
import           Data.Aeson.TH (deriveToJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Core.Configuration.Core (CoreConfiguration (..))
import           Pos.Crypto (RequiresNetworkMagic (..))

deriveToJSON defaultOptions ''CoreConfiguration

instance FromJSON CoreConfiguration where
    parseJSON = withObject "core" $ \obj -> do
        ccg   <- obj .: "genesis"
        ccdsv <- obj .: "dbSerializeVersion"
        -- If "requiresNetworkMagic" is not specified, default to RequiresMagic
        ccrnm <- obj .:? "requiresNetworkMagic" .!= RequiresMagic
        pure $ CoreConfiguration ccg ccdsv ccrnm
