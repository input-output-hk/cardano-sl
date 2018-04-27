module Cardano.Wallet.API.V1.Handlers (handlers) where

import Universum
import Servant

import Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import qualified Cardano.Wallet.API.V1 as V1

handlers :: forall m. ActiveWalletLayer m -> Server V1.API
handlers _w = error "TODO"
