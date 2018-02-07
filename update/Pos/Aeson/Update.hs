module Pos.Aeson.Update
    (
    ) where

import           Universum

import           Data.Aeson (FromJSON (..))
import           Data.Aeson.TH (deriveToJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Core.Update (SystemTag (..))

instance FromJSON SystemTag where
    parseJSON v = UnsafeSystemTag <$> parseJSON v

deriveToJSON defaultOptions ''SystemTag
