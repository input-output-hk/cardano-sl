-- | Function related to Mpc messages.

module Pos.Types.Mpc
       ( verifyCommitment
       , verifyOpening
       ) where

import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Crypto          (verifyEncShare, verifySecretProof)
import           Pos.Types.Types     (Commitment (..), Opening (..))

-- | Verify that Commitment is correct.
verifyCommitment :: Commitment -> Bool
verifyCommitment Commitment {..} = all verifyCommitmentDo $ HM.toList commShares
  where
    verifyCommitmentDo = uncurry (verifyEncShare commExtra)

-- | Verify that Secret provided with Opening corresponds to given commitment.
verifyOpening :: Commitment -> Opening -> Bool
verifyOpening Commitment {..} (Opening secret) =
    verifySecretProof commExtra secret commProof
