-- | Wallet-specific types

-- TODO [CSM-407] Name was preserved for compatibility with other PRs,
-- better to rename to just 'Types' or smth like that.

module Pos.Wallet.Web.ClientTypes
       (
         module Pos.Wallet.Web.ClientTypes.Functions
       , module Pos.Wallet.Web.ClientTypes.Types
       ) where

import           Pos.Wallet.Web.ClientTypes.Functions
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.ClientTypes.Types
