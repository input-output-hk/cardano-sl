-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class     (Bi (..), label, labelS, putField)
import           Pos.Binary.Core      ()
import           Pos.Binary.Crypto    ()
import           Pos.Delegation.Types (DlgPayload (getDlgPayload), mkDlgPayload)
import           Pos.Util.Util        (eitherToFail)
import qualified Pos.Binary.Cbor      as Cbor

instance Bi DlgPayload where
    sizeNPut = labelS "DlgPayload" $ putField getDlgPayload
    get = label "DlgPayload" $ eitherToFail . mkDlgPayload =<< get

instance Cbor.Bi DlgPayload where
  encode = Cbor.encode . getDlgPayload
  decode = Cbor.decode >>= eitherToFail . mkDlgPayload
