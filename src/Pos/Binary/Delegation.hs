-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class     (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core      ()
import           Pos.Binary.Crypto    ()
import           Pos.Core             (ProxySKHeavy, StakeholderId)
import           Pos.Delegation.Types (DlgPayload (getDlgPayload), DlgUndo (..),
                                       mkDlgPayload)
import           Pos.Util.Util        (eitherToFail)

instance Bi DlgPayload where
    encode = encode . getDlgPayload
    decode = decode >>= eitherToFail . mkDlgPayload

deriveSimpleBi ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]
