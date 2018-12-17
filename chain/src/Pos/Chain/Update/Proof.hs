module Pos.Chain.Update.Proof
       ( UpdateProof
       , mkUpdateProof
       ) where

import           Pos.Crypto (Hash, hash)

import           Pos.Chain.Update.Payload

-- | Proof that body of update message contains 'UpdatePayload'.
type UpdateProof = Hash UpdatePayload

mkUpdateProof :: UpdatePayload -> UpdateProof
mkUpdateProof = hash
