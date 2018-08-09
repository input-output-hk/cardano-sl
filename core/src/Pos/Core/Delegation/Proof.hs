module Pos.Core.Delegation.Proof
       ( DlgProof
       , mkDlgProof
       ) where

import           Pos.Binary.Class (Bi)
import           Pos.Crypto (Hash, hash)

import           Pos.Core.Delegation.Payload

-- | Proof of delegation payload.
type DlgProof = Hash DlgPayload

-- | Creates 'DlgProof' out of delegation payload.
mkDlgProof :: Bi DlgPayload => DlgPayload -> DlgProof
mkDlgProof = hash
