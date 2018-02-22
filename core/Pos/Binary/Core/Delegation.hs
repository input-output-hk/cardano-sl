-- | Delegation types serialization.

module Pos.Binary.Core.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation (DlgPayload (..))

instance HasConfiguration => Bi DlgPayload where
    encode = encode . getDlgPayload
    decode = UnsafeDlgPayload <$> decode
