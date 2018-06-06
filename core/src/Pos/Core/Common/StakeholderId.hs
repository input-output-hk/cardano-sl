module Pos.Core.Common.StakeholderId
       ( StakeholderId
       ) where

import           Pos.Crypto.Signing (PublicKey)

import           Pos.Core.Common.AddressHash

-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey
