{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Aeson.Update
    (
    ) where

import           Data.Aeson (FromJSON (..))
import           Data.Aeson.TH (deriveToJSON)
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

import           Pos.Core.Update (SystemTag (..))

instance FromJSON SystemTag where
    parseJSON v = SystemTag <$> parseJSON v

deriveToJSON defaultOptions ''SystemTag
