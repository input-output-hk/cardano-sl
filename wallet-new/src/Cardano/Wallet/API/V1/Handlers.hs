module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)

handlers :: forall m. ActiveWalletLayer m -> Server V1.API
handlers _w = error "TODO"
