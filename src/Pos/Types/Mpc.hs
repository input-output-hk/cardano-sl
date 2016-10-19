-- | Function related to Mpc messages.

module Pos.Types.Mpc
       ( verifyOpening
       ) where

import           Universum

import           Pos.Crypto      (verifySecretProof)
import           Pos.Types.Types (Commitment (..), Opening (..))

verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) =
    verifySecretProof commExtra secret commProof
