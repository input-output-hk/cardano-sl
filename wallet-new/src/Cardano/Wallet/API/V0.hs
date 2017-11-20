
module Cardano.Wallet.API.V0 where

import qualified Pos.Wallet.Web.Api as V0

-- | "Mount" the legacy API here.
type API = V0.WalletApiNoPrefix
