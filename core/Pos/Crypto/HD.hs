-- | Hierarchical derivation interface.

module Pos.Crypto.HD
       ( HDPassphrase
       , HDAddressPayload (..)
       ) where

import           Universum

newtype HDPassphrase = HDPassphrase ByteString

newtype HDAddressPayload = HDAddressPayload ByteString
    deriving (Eq, Ord, Show, NFData, Generic)
