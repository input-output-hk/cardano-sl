-- | Wallet-specific types

-- TODO [CSM-407] Name was preserved for compatibility with other PRs,
-- better to rename to just 'Types' or smth like that.

module Cardano.Wallet.Compatibility.ClientTypes
       (
         module Cardano.Wallet.Compatibility.ClientTypes.Functions
       , module Cardano.Wallet.Compatibility.ClientTypes.Types
       ) where

import           Cardano.Wallet.Compatibility.ClientTypes.Functions
import           Cardano.Wallet.Compatibility.ClientTypes.Instances ()
import           Cardano.Wallet.Compatibility.ClientTypes.Types
