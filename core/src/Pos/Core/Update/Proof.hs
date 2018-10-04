module Pos.Core.Update.Proof
       ( UpdateProof
       , mkUpdateProof
       ) where

import           Pos.Crypto (Hash, hash)

import           Pos.Core.Update.Payload

-- | Proof that body of update message contains 'UpdatePayload'.
type UpdateProof = Hash UpdatePayload

mkUpdateProof :: UpdatePayload -> UpdateProof
mkUpdateProof = hash
