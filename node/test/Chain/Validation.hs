-- | Validation of chain extension.
--
--   This module contains types and functions relevant to the question of, given
--   an existing chain C and a new block B, is that block a valid extension of
--   the chain? Various additional state may also be needed to determine this.
module Chain.Validation where

import Chain.Abstract
import Chain.Validation.Parameters
import qualified UTxO.DSL as DSL
import Universum

data ValidationState = ValidationState
  { -- | Slot currently being validated.
    vsCurrentSlot :: SlotId

  }

-- | Encapsulate access to chain parameters.
newtype WithChainParameters a = WithChainParameters
  { runWithChainParameters :: Reader (Parameters ValidationState DSL.IdentityAsHash Addr) a }


newtype Validation e a = Validation
  { runValidation :: StateT ValidationState (ExceptT e WithChainParameters) a}
