{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pos.Aeson.Crypto
       (
       ) where

import           Data.Aeson (ToJSON (toJSON))
import           Formatting (sformat)
import           Pos.Crypto (Hash, PublicKey, fullPublicKeyF)

import           Universum

instance ToJSON (Hash a) where
    toJSON = toJSON . pretty

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF
