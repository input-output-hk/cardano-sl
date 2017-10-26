-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class     (Bi (..), Cons (..), Field (..), dcNoCheck,
                                       deriveSimpleBi)
import           Pos.Binary.Core      ()
import           Pos.Binary.Crypto    ()
import           Pos.Core             (HasConfiguration, ProxySKHeavy, StakeholderId)
import           Pos.Delegation.Types (DlgPayload (..), DlgUndo (..), mkDlgPayload)
import           Pos.Util.Util        (eitherToFail)

instance HasConfiguration => Bi DlgPayload where
    encode = encode . getDlgPayload
    decode = do
        psks <- decode
        ifM (view dcNoCheck)
            (pure $ UnsafeDlgPayload psks)
            (eitherToFail $ mkDlgPayload psks)

deriveSimpleBi ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]
