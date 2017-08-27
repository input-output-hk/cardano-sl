module Pos.Aeson.Crypto
       (
       ) where

import           Universum

import           Crypto.Hash   (HashAlgorithm)
import           Data.Aeson    (FromJSON (..), ToJSON (..))
import           Formatting    (sformat)

import           Pos.Crypto    (AbstractHash, PublicKey, fullPublicKeyF, hashHexF)
import           Pos.Util.Util (parseJSONWithRead)

instance ToJSON (AbstractHash algo a) where
    toJSON = toJSON . sformat hashHexF

instance HashAlgorithm algo => FromJSON (AbstractHash algo a) where
    parseJSON = parseJSONWithRead

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF
