module Pos.Crypto.Configuration
       ( ProtocolMagic (..)
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.SafeCopy (base, deriveSafeCopySimple)

-- | Magic number which should differ for different clusters. It's
-- defined here, because it's used for signing. It also used for other
-- things (e. g. it's part of a serialized block).
newtype ProtocolMagic = ProtocolMagic
    { getProtocolMagic :: Int32
    } deriving (Show, Eq, NFData)

deriving instance ToJSON ProtocolMagic
deriving instance FromJSON ProtocolMagic

deriveSafeCopySimple 0 'base ''ProtocolMagic
