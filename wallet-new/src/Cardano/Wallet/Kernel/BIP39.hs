{-- | This module trivially re-exports everything from 'Pos.Util.Mnemonic',
      in preparation for the final deprecation of 'wallet' code once the new
      data layer will be delivered.
--}

module Cardano.Wallet.Kernel.BIP39 (
    module LegacyCompat
    ) where

import           Pos.Util.Mnemonic as LegacyCompat


