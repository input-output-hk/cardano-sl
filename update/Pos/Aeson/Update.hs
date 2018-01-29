module Pos.Aeson.Update
    (
    ) where

import           Data.Aeson (FromJSON (..))
import           Data.Aeson.TH (deriveToJSON)
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

import           Pos.Core.Update (SystemTag, mkSystemTag)
import           Pos.Util (toAesonError)

instance FromJSON SystemTag where
    parseJSON v = parseJSON v >>= toAesonError . mkSystemTag

deriveToJSON defaultOptions ''SystemTag
