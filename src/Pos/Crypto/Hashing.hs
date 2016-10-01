-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( Hash
       , hashlazy
       ) where

import           Crypto.Hash (Digest, SHA256, hashlazy)

type Hash = Digest SHA256
