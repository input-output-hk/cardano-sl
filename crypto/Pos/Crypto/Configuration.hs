-- | Configuration of cryptographic primitives.

{-# LANGUAGE RankNTypes #-}

module Pos.Crypto.Configuration
       ( ProtocolMagic (..)
       , HasProtocolMagic
       , withProtocolMagic
       , protocolMagic
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Reflection (Given (..), give, given)

-- | Magic number which should differ for different clusters. It's
-- defined here, because it's used for signing. It also used for other
-- things (e. g. it's part of a serialized block).
newtype ProtocolMagic = ProtocolMagic
    { getProtocolMagic :: Int32
    } deriving (Show, Eq, NFData)

deriving instance ToJSON ProtocolMagic
deriving instance FromJSON ProtocolMagic

type HasProtocolMagic = Given ProtocolMagic

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish different clusters. It's an example, possible usages
-- are wider.
protocolMagic :: HasProtocolMagic => ProtocolMagic
protocolMagic = given

withProtocolMagic :: ProtocolMagic -> (HasProtocolMagic => r) -> r
withProtocolMagic = give
