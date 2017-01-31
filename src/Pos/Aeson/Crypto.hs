{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.Crypto
       (
       ) where

import           Data.Aeson (ToJSON (toJSON))
import           Formatting (sformat)
import           Pos.Crypto (AbstractHash, PublicKey, fullPublicKeyF, hashHexF)

import           Universum

instance ToJSON (AbstractHash algo a) where
    toJSON = toJSON . sformat hashHexF

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF
