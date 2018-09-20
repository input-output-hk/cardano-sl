{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Derived FromJSON instance for Pos.Core.Configuration.Configuration

module Pos.Aeson.Core.Configuration
    (
    ) where

import           Universum

import           Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import           Data.Aeson.TH (deriveToJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Core.Configuration.Core (CoreConfiguration (..))
import           Pos.Crypto (RequiresNetworkMagic (..))

deriveToJSON defaultOptions ''CoreConfiguration

instance FromJSON CoreConfiguration where
    parseJSON = withObject "core" $ \obj -> do
        ccg   <- obj .: "genesis"
        ccdsv <- obj .: "dbSerializeVersion"
        ccrnm <- determineRNM <$> obj .:? "requiresNetworkMagic"
        pure $ CoreConfiguration ccg ccdsv ccrnm
      where
        -- If "requiresNetworkMagic" is not specified, default of NMMustBeJust
        determineRNM :: Maybe RequiresNetworkMagic -> RequiresNetworkMagic
        determineRNM mrnm = case mrnm of
            Nothing -> NMMustBeJust
            Just x  -> x
