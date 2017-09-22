module Pos.Aeson.Update
    (
    ) where

import           Universum
import           Data.Aeson             (FromJSON (..))
import           Data.Aeson.TH          (deriveToJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Update.Core.Types (SystemTag, mkSystemTag)

instance FromJSON SystemTag where
    parseJSON v = parseJSON v >>= mkSystemTag

deriveToJSON defaultOptions ''SystemTag
