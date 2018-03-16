module Cardano.Wallet.API.V0 where

import           Cardano.Wallet.API.Types
import           Servant ((:>))

import qualified Pos.Wallet.Web.Api as V0

-- | "Mount" the legacy API here.
type API = Tags '["V0 (Deprecated)"] :> V0.WalletApiNoPrefix
