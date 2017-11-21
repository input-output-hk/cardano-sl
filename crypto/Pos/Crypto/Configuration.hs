-- | Configuration of cryptographic primitives.

module Pos.Crypto.Configuration
       ( ProtocolMagic (..)
       , HasCryptoConfiguration
       , protocolMagic
       ) where

import           Universum

import           Data.Reflection (Given (..), given)

-- | Magic number which should differ for different clusters. It's
-- defined here, because it's used for signing. It also used for other
-- things (e. g. it's part of a serialized block).
newtype ProtocolMagic = ProtocolMagic
    { getProtocolMagic :: Int32
    } deriving (Show, Eq)

type HasCryptoConfiguration = Given ProtocolMagic

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish different clusters. It's an example, possible usages
-- are wider.
protocolMagic :: HasCryptoConfiguration => ProtocolMagic
protocolMagic = given
