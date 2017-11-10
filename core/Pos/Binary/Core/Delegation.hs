-- | Delegation types serialization.

module Pos.Binary.Core.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation (DlgPayload (getDlgPayload), mkDlgPayload)
import           Pos.Util.Util (eitherToFail)

instance HasConfiguration => Bi DlgPayload where
    encode = encode . getDlgPayload
    decode = decode >>= eitherToFail . mkDlgPayload
