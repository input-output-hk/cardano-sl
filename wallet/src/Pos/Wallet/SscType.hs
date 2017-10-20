-- | Global export of the @SSC@ type. Exported in order to be able to switch to
-- different type-level implementations.

module Pos.Wallet.SscType
       ( WalletSscType
       ) where

import           Pos.Security.Workers ()
import           Pos.Ssc.GodTossing   (SscGodTossing)
--import Pos.Ssc.NistBeacon (SscNistBeacon)

-- | We can also use CPP flags that I proposed that would enable switching between
-- implementations without changing the code.
type WalletSscType = SscGodTossing
--type WalletSscType = SscNistBeacon
