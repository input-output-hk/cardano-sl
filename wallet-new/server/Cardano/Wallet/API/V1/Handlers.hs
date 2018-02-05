module Cardano.Wallet.API.V1.Handlers (handlers) where

import Universum
import Servant

import Cardano.Wallet.Kernel
import qualified Cardano.Wallet.API.V1 as V1

handlers :: ActiveWallet -> Server V1.API
handlers _w = undefined
